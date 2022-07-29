use super::Parser;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Default)]
pub(super) struct ParserState {
    pub(super) qualified_rule_ctx: Option<QualifiedRuleContext>,
}

#[derive(Clone)]
pub(super) enum QualifiedRuleContext {
    Selector,
    DeclarationName,
    DeclarationValue,
}

impl<'a> Parser<'a> {
    pub(super) fn with_state(&mut self, state: ParserState) -> WithState<'a, '_> {
        let original_state = self.state.clone();
        self.state = state;
        WithState {
            parser: self,
            original_state,
        }
    }
}

pub(super) struct WithState<'a, 'b> {
    parser: &'b mut Parser<'a>,
    original_state: ParserState,
}

impl<'a, 'b> Deref for WithState<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> DerefMut for WithState<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

impl<'a, 'b> Drop for WithState<'a, 'b> {
    fn drop(&mut self) {
        self.state = self.original_state.clone();
    }
}
