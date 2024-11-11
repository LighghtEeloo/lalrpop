//! LSP interface for LALRPOP
//!
//! This module provides a Language Server Protocol (LSP) interface for LALRPOP.
//! Supported features include:
//!
//! - Syntax highlighting
//! - Go to definition
//! - Find references

use crate::grammar::parse_tree as pt;
use crate::grammar::repr as r;
use crate::normalize;
use crate::parser;
use crate::session::Session;
use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

/// Text document item for file changes
pub struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

/// LALRPOP Language Server Protocol
pub struct LalrpopLsp {
    client: Client,
    parse_trees: DashMap<String, pt::Grammar>,
    repr: DashMap<String, r::Grammar>,
}

impl LalrpopLsp {
    /// Create a new LALRPOP Language Server Protocol
    pub fn new(client: Client) -> Self {
        Self {
            client,
            parse_trees: DashMap::new(),
            repr: DashMap::new(),
        }
    }
    /// Get the grammar for a given URI
    pub async fn on_change(&self, params: TextDocumentItem) {
        let grammar: pt::Grammar = parser::parse_grammar(params.text.as_str()).unwrap();
        self.parse_trees
            .insert(params.uri.to_string(), grammar.to_owned());
        let grammar: r::Grammar = normalize::normalize(&Session::new(), grammar).unwrap();
        self.repr.insert(params.uri.to_string(), grammar);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for LalrpopLsp {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
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
}
