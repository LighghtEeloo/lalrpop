//! LSP interface for LALRPOP
//!
//! This module provides a Language Server Protocol (LSP) interface for LALRPOP.
//! Supported features include:
//!
//! - Syntax highlighting
//! - Go to definition
//! - Find references

use std::collections::HashMap;
use std::path::PathBuf;

use crate::file_text::FileText;
use crate::grammar::parse_tree as pt;
use crate::grammar::parse_tree::{Alternative, ExprSymbol, Symbol};
use crate::grammar::repr as r;
use crate::normalize;
use crate::parser;
use crate::session::Session;

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

/// Info extracted from a LALRPOP file.
#[allow(dead_code)]
pub struct LalrpopFile {
    file_text: FileText,
    tree: pt::Grammar,
    /// Spanned items in the file.
    pub span_items: Vec<(Span, SpanItem)>,
    /// Span of definitions.
    pub definitions: HashMap<String, Span>,
    /// Span of references.
    pub references: HashMap<String, Vec<Span>>,
    repr: r::Grammar,
}

impl LalrpopFile {
    /// Constructor.
    pub fn new(text: impl AsRef<str>) -> Self {
        let file_text = FileText::new(PathBuf::new(), text.as_ref().to_owned());
        let tree: pt::Grammar = parser::parse_grammar(text.as_ref()).unwrap();
        let mut span_items = vec![];
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
                        args: _,
                        type_decl: _,
                        alternatives,
                    }) => {
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
        let repr: r::Grammar = normalize::normalize(&Session::new(), tree.to_owned()).unwrap();
        Self {
            file_text,
            tree,
            span_items,
            definitions,
            references,
            repr,
        }
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
