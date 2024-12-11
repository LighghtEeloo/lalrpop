//! LSP interface for LALRPOP
//!
//! This module provides a Language Server Protocol (LSP) interface for LALRPOP.
//! Supported features include:
//!
//! - Syntax highlighting
//! - Go to definition
//! - Find references

use {
    crate::{
        file_text::FileText,
        grammar::{
            parse_tree::{self as pt, Alternative, ExprSymbol, Symbol},
            repr as r,
        },
        normalize::{self, NormError},
        parser::{self, ParseError},
        session::Session,
        tok,
    },
    std::{collections::HashMap, io, path::PathBuf},
};

pub use crate::grammar::parse_tree::Span;

/// Items that can be found with a span.
#[derive(Clone, Debug)]
pub enum SpanItem {
    /// Grammar of the LALRPOP file.
    Grammar,
    /// Definition of a nonterminal or macro.
    Definition(String),
    /// Reference to a nonterminal or macro.
    Reference(String),
}

/// Type annotation of nonterminal and macros.
pub struct TypeDecl {
    /// Arguments.
    pub args: Vec<String>,
    /// Return type.
    pub ret: Option<pt::TypeRef>,
}

/// Info extracted from a LALRPOP file.
#[allow(dead_code)]
pub struct LalrpopFile {
    /// The file text, with newline and span info.
    file_text: FileText,
    tree: pt::Grammar,
    /// Spanned items in the file.
    pub span_items: Vec<(Span, SpanItem)>,
    /// Span of definitions.
    pub definitions: HashMap<String, Span>,
    /// Type declarations of definitions.
    pub definition_type_decls: HashMap<String, TypeDecl>,
    /// Span of references.
    pub references: HashMap<String, Vec<Span>>,
    repr: r::Grammar,
}

impl LalrpopFile {
    /// Constructor.
    pub fn new(text: impl AsRef<str>) -> Result<Self, DiagnosticError> {
        let file_text = FileText::new(PathBuf::new(), text.as_ref().to_owned());
        let tree: pt::Grammar =
            parser::parse_grammar(text.as_ref()).map_err(|e| report_parse_error(&file_text, e))?;
        let mut span_items = Vec::new();
        let mut definition_type_decls = HashMap::new();
        {
            let tree = &tree;
            // traverse the tree to extract spans
            let pt::Grammar {
                prefix: _,
                span,
                type_parameters: _,
                parameters: _,
                where_clauses: _,
                items,
                attributes: _,
                module_attributes: _,
            } = tree;
            let span = span.to_owned();
            span_items.push((span, SpanItem::Grammar));
            for item in items {
                match item {
                    pt::GrammarItem::MatchToken(_match_token) => {
                        // not related to definition or reference feature, skipping
                    }
                    pt::GrammarItem::ExternToken(_extern_token) => {
                        // not related to definition or reference feature, skipping
                    }
                    pt::GrammarItem::InternToken(_intern_token) => {
                        // not related to definition or reference feature, skipping
                    }
                    pt::GrammarItem::Nonterminal(pt::NonterminalData {
                        visibility: _,
                        name,
                        attributes: _,
                        span,
                        // we can't do anything with just strings
                        args,
                        type_decl,
                        alternatives,
                    }) => {
                        definition_type_decls.insert(
                            name.to_string(),
                            TypeDecl {
                                args: args.iter().map(|arg| arg.to_string()).collect(),
                                ret: type_decl.clone(),
                            },
                        );
                        let span = span.to_owned();
                        span_items.push((span, SpanItem::Definition(name.to_string())));
                        for alternative in alternatives {
                            let Alternative {
                                // don't care about the whole alternative
                                span: _,
                                expr,
                                // `if` guard, only legal in macros
                                condition: _,
                                // code blocks afterwards
                                action: _,
                                attributes: _,
                            } = alternative;
                            Self::collect_expr_symbol(expr, &mut span_items);
                        }
                    }
                    pt::GrammarItem::Use(_) => {
                        // what is use? we don't deal with it here
                    }
                }
            }
            // next, sort the spans
            span_items.sort_by_key(|(span, _)| span.0);
        }
        let definitions = span_items
            .iter()
            .filter_map(|(span, item)| match item {
                SpanItem::Definition(name) => Some((name.to_string(), span.to_owned())),
                _ => None,
            })
            .collect();
        let references = {
            let mut references = HashMap::new();
            for (span, item) in span_items.iter() {
                match item {
                    SpanItem::Reference(name) => {
                        references
                            .entry(name.to_string())
                            .or_insert_with(Vec::new)
                            .push(span.to_owned());
                    }
                    _ => {}
                }
            }
            references
        };
        let repr: r::Grammar = normalize::normalize(&Session::new(), tree.to_owned()).map_err(
            |NormError {
                 message,
                 span: Span(lo, hi),
             }| DiagnosticError {
                loc: ErrorLoc::Span {
                    lo: file_text.line_col(lo),
                    hi: file_text.line_col(hi),
                },
                message,
                io_error: io::Error::from(io::ErrorKind::InvalidData),
            },
        )?;
        Ok(Self {
            file_text,
            tree,
            span_items,
            definitions,
            references,
            definition_type_decls,
            repr,
        })
    }

    fn collect_expr_symbol(expr_symbol: &ExprSymbol, span_items: &mut Vec<(Span, SpanItem)>) {
        let ExprSymbol { symbols } = expr_symbol;
        for symbol in symbols {
            Self::collect_symbol(symbol, span_items);
        }
    }

    fn collect_symbol(symbol: &Symbol, span_items: &mut Vec<(Span, SpanItem)>) {
        let Symbol { span, kind } = symbol;
        let span = span.to_owned();
        match kind {
            pt::SymbolKind::Expr(expr_symbol) => Self::collect_expr_symbol(expr_symbol, span_items),
            pt::SymbolKind::AmbiguousId(atom) => {
                // figure out what to do with this
                span_items.push((span, SpanItem::Reference(atom.to_string())));
            }
            pt::SymbolKind::Terminal(_terminal_string) => {
                // terminal, nothing to do
            }
            pt::SymbolKind::Nonterminal(_nonterminal_string) => {
                // nonterminal, don't know what to do with it
            }
            pt::SymbolKind::Macro(macro_symbol) => {
                let pt::MacroSymbol { name, args } = macro_symbol;
                span_items.push((span, SpanItem::Reference(name.to_string())));
                for arg in args {
                    Self::collect_symbol(arg, span_items);
                }
            }
            pt::SymbolKind::Repeat(repeat_symbol) => {
                Self::collect_symbol(&repeat_symbol.symbol, span_items);
            }
            pt::SymbolKind::Choose(symbol) => {
                Self::collect_symbol(symbol, span_items);
            }
            pt::SymbolKind::Name(_name, symbol) => {
                Self::collect_symbol(symbol, span_items);
            }
            pt::SymbolKind::Lookahead => {}
            pt::SymbolKind::Lookbehind => {}
            pt::SymbolKind::Error => {}
        }
    }

    /// Get the offset from a line and column.
    pub fn offset_from_line_col(&self, line: usize, col: usize) -> Option<usize> {
        self.file_text.offset_from_line_col(line, col)
    }

    /// Get the line and column from an offset.
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        self.file_text.line_col(offset)
    }

    /// Find spans that contain a given offset.
    pub fn hit_offset_in_spans(&self, offset: usize) -> Vec<(Span, SpanItem)> {
        self.span_items
            .iter()
            .filter(|(span, _)| span.0 <= offset && offset <= span.1)
            .cloned()
            .collect()
    }

    /// Filter span hits to the closest one.
    pub fn closest_hit(hits: Vec<(Span, SpanItem)>) -> Option<(Span, SpanItem)> {
        hits.into_iter().min_by_key(|(span, _)| span.1 - span.0)
    }
}

/// Error location.
pub enum ErrorLoc {
    /// Pointwise error, line and column.
    Point(usize, usize),
    /// Range error.
    Span {
        /// Low.
        lo: (usize, usize),
        /// Hi.
        hi: (usize, usize),
    },
}

/// Diagnostic error.
pub struct DiagnosticError {
    /// Location of the error.
    pub loc: ErrorLoc,
    /// Error message.
    pub message: String,
    /// IO error.
    pub io_error: io::Error,
}

fn report_parse_error<'input>(file_text: &FileText, error: ParseError<'input>) -> DiagnosticError {
    let mut message = String::new();
    let mut report_error = |file_text: &FileText, span: pt::Span, msg: &str| {
        use std::fmt::Write;

        writeln!(&mut message, "{}", msg).unwrap();

        file_text.highlight_fmt(span, &mut message).unwrap();
    };

    // copy-pasted from lalrpop/src/build/mod.rs:271:1 for now;
    // we need a better error reporting abstraction

    match error {
        (ParseError::InvalidToken { location }) => {
            let loc = {
                let (line, col) = file_text.line_col(location);
                ErrorLoc::Point(line, col)
            };
            let ch = file_text.text()[location..].chars().next().unwrap();
            report_error(
                file_text,
                pt::Span(location, location),
                &format!("invalid character `{}`", ch),
            );
            DiagnosticError {
                loc,
                message,
                io_error: io::Error::from(io::ErrorKind::InvalidData),
            }
        }

        (ParseError::UnrecognizedEof { location, .. }) => {
            let (line, col) = file_text.line_col(location);
            let loc = ErrorLoc::Point(line, col);
            report_error(
                file_text,
                pt::Span(location, location),
                "unexpected end of file",
            );
            DiagnosticError {
                loc,
                message,
                io_error: io::Error::from(io::ErrorKind::UnexpectedEof),
            }
        }

        (ParseError::UnrecognizedToken {
            token: (lo, _, hi),
            expected,
        }) => {
            let _ = expected; // didn't implement this yet :)
            let loc = ErrorLoc::Span {
                lo: file_text.line_col(lo),
                hi: file_text.line_col(hi),
            };
            let text = &file_text.text()[lo..hi];
            report_error(
                file_text,
                pt::Span(lo, hi),
                &format!("unexpected token: `{}`", text),
            );
            DiagnosticError {
                loc,
                message,
                io_error: io::Error::from(io::ErrorKind::InvalidData),
            }
        }

        (ParseError::ExtraToken { token: (lo, _, hi) }) => {
            let loc = ErrorLoc::Span {
                lo: file_text.line_col(lo),
                hi: file_text.line_col(hi),
            };
            let text = &file_text.text()[lo..hi];
            report_error(
                file_text,
                pt::Span(lo, hi),
                &format!("extra token at end of input: `{}`", text),
            );
            DiagnosticError {
                loc,
                message,
                io_error: io::Error::from(io::ErrorKind::InvalidData),
            }
        }

        (ParseError::User { error }) => {
            let string = match error.code {
                tok::ErrorCode::UnrecognizedToken => "unrecognized token",
                tok::ErrorCode::UnterminatedEscape => "unterminated escape; missing '`'?",
                tok::ErrorCode::UnrecognizedEscape => {
                    "unrecognized escape; only \\n, \\r, \\t, \\\" and \\\\ are recognized"
                }
                tok::ErrorCode::UnterminatedStringLiteral => {
                    "unterminated string literal; missing `\"`?"
                }
                tok::ErrorCode::UnterminatedCharacterLiteral => {
                    "unterminated character literal; missing `'`?"
                }
                tok::ErrorCode::UnterminatedAttribute => "unterminated #! attribute; missing `]`?",
                tok::ErrorCode::ExpectedStringLiteral => "expected string literal; missing `\"`?",
                tok::ErrorCode::UnterminatedCode => {
                    "unterminated code block; perhaps a missing `;`, `)`, `]` or `}`?"
                }
                tok::ErrorCode::UnterminatedBlockComment => {
                    "unterminated block comment; missing `*/`?"
                }
            };

            let loc = {
                let (line, col) = file_text.line_col(error.location);
                ErrorLoc::Point(line, col)
            };
            report_error(
                file_text,
                pt::Span(error.location, error.location + 1),
                string,
            );
            DiagnosticError {
                loc,
                message,
                io_error: io::Error::from(io::ErrorKind::InvalidData),
            }
        }
    }
}
