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
use crate::grammar::parse_tree::{Alternative, ExprSymbol, Span, Symbol};
use crate::grammar::repr as r;
use crate::normalize;
use crate::parser;
use crate::session::Session;
use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

/// Text document item for file changes
#[allow(dead_code)]
pub struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

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
    span_items: Vec<(Span, SpanItem)>,
    definitions: HashMap<String, Span>,
    references: HashMap<String, Vec<Span>>,
    repr: r::Grammar,
}

impl LalrpopFile {
    /// Constructor.
    pub fn new(text: impl AsRef<str>) -> Self {
        let file_text = FileText::new(PathBuf::new(), text.as_ref().to_owned());
        let tree: pt::Grammar = parser::parse_grammar(text.as_ref()).unwrap();
        let mut span_items = vec![];
        {
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
            } = tree.to_owned();
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

    fn collect_expr_symbol(expr_symbol: ExprSymbol, span_items: &mut Vec<(Span, SpanItem)>) {
        let ExprSymbol { symbols } = expr_symbol;
        for symbol in symbols {
            Self::collect_symbol(symbol, span_items);
        }
    }

    fn collect_symbol(symbol: Symbol, span_items: &mut Vec<(Span, SpanItem)>) {
        let Symbol { span, kind } = symbol;
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
                Self::collect_symbol(repeat_symbol.symbol, span_items);
            }
            pt::SymbolKind::Choose(symbol) => {
                Self::collect_symbol(*symbol, span_items);
            }
            pt::SymbolKind::Name(_name, symbol) => {
                Self::collect_symbol(*symbol, span_items);
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

    fn offset_to_position(&self, offset: usize) -> Position {
        let (line, col) = self.line_col(offset);
        Position {
            line: line as u32,
            character: col as u32,
        }
    }
}

/// LALRPOP Language Server Protocol
pub struct LalrpopLsp {
    client: Client,
    files: DashMap<String, LalrpopFile>,
}

impl LalrpopLsp {
    /// Create a new LALRPOP Language Server Protocol
    pub fn new(client: Client) -> Self {
        Self {
            client,
            files: DashMap::new(),
        }
    }
    /// Get the grammar for a given URI
    pub async fn on_change(&self, params: TextDocumentItem) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("on change: {}", params.uri.as_str()),
                // format!("on change:\n{}", params.text.as_str()),
            )
            .await;

        let uri = params.uri.to_string();
        let file = LalrpopFile::new(params.text.as_str());

        // self.client
        //     .log_message(MessageType::INFO, format!("parsed:\n{:#?}", file.tree))
        //     .await;
        self.client
            .log_message(
                MessageType::INFO,
                format!("spanned:\n{:#?}", file.span_items),
            )
            .await;
        // self.client
        //     .log_message(MessageType::INFO, format!("normalized:\n{:#?}", file.repr))
        //     .await;

        // update
        self.files.insert(uri.clone(), file);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for LalrpopLsp {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                // semantic_tokens_provider: Some(
                //     SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                //         SemanticTokensRegistrationOptions {
                //             text_document_registration_options: {
                //                 TextDocumentRegistrationOptions {
                //                     document_selector: Some(vec![DocumentFilter {
                //                         language: Some("LALRPOP".to_string()),
                //                         scheme: Some("file".to_string()),
                //                         pattern: None,
                //                     }]),
                //                 }
                //             },
                //             semantic_tokens_options: SemanticTokensOptions {
                //                 work_done_progress_options: WorkDoneProgressOptions::default(),
                //                 legend: SemanticTokensLegend {
                //                     // token_types: `LEGEND_TYPE`.into(),
                //                     token_types: [].into(),
                //                     token_modifiers: vec![],
                //                 },
                //                 range: Some(true),
                //                 full: Some(SemanticTokensFullOptions::Bool(true)),
                //             },
                //             static_registration_options: StaticRegistrationOptions::default(),
                //         },
                //     ),
                // ),
                ..Default::default()
            },
            ..Default::default()
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let Some(file) = self.files.get(uri.as_str()) else {
            return Ok(None);
        };
        let position = params.text_document_position_params.position;
        let Some(offset) =
            file.offset_from_line_col(position.line as usize, position.character as usize)
        else {
            return Ok(None);
        };
        let hits = file.hit_offset_in_spans(offset);
        // self.client
        //     .log_message(
        //         MessageType::INFO,
        //         format!("goto definition hits: {:#?}", hits),
        //     )
        //     .await;
        let Some((_span, span_item)) = LalrpopFile::closest_hit(hits) else {
            return Ok(None);
        };
        match span_item {
            SpanItem::Grammar => {}
            SpanItem::Definition(def) => {
                // Todo: actually we return the references here
                let Some(spans) = file.references.get(&def) else {
                    return Ok(None);
                };
                return Ok(Some(GotoDefinitionResponse::Array(
                    spans
                        .into_iter()
                        .map(|span| {
                            let start = file.offset_to_position(span.0);
                            let end = file.offset_to_position(span.1);
                            Location {
                                uri: uri.to_owned(),
                                range: Range { start, end },
                            }
                        })
                        .collect(),
                )));
            }
            SpanItem::Reference(def) => {
                let Some(span) = file.definitions.get(&def) else {
                    return Ok(None);
                };
                let start = file.offset_to_position(span.0);
                let end = file.offset_to_position(span.1);
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri,
                    range: Range { start, end },
                })));
            }
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let Some(file) = self.files.get(uri.as_str()) else {
            return Ok(None);
        };
        let position = params.text_document_position.position;
        let Some(offset) =
            file.offset_from_line_col(position.line as usize, position.character as usize)
        else {
            return Ok(None);
        };
        let hits = file.hit_offset_in_spans(offset);
        // self.client
        //     .log_message(
        //         MessageType::INFO,
        //         format!("references hits: {:#?}", hits),
        //     )
        //     .await;
        let Some((_span, span_item)) = LalrpopFile::closest_hit(hits) else {
            return Ok(None);
        };
        match span_item {
            SpanItem::Grammar => {}
            SpanItem::Reference(_) => {}
            SpanItem::Definition(def) => {
                let Some(spans) = file.references.get(&def) else {
                    return Ok(None);
                };
                return Ok(Some(
                    spans
                        .into_iter()
                        .map(|span| {
                            let start = file.offset_to_position(span.0);
                            let end = file.offset_to_position(span.1);
                            Location {
                                uri: uri.to_owned(),
                                range: Range { start, end },
                            }
                        })
                        .collect(),
                ));
            }
        }
        Ok(None)
    }
}
