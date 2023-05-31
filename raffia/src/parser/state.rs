use super::Parser;
use std::{
    mem,
    ops::{Deref, DerefMut},
};

#[derive(Clone, Debug, Default)]
pub(super) struct ParserState {
    pub(super) qualified_rule_ctx: Option<QualifiedRuleContext>,
    pub(super) in_sass_function: bool,
}

#[derive(Clone, Debug)]
pub(super) enum QualifiedRuleContext {
    Selector,
    DeclarationName,
    DeclarationValue,
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn with_state(&mut self, state: ParserState) -> WithState<'cmt, 's, '_> {
        let original_state = mem::replace(&mut self.state, state);
        WithState {
            parser: self,
            original_state,
        }
    }
}

pub(super) struct WithState<'cmt, 's: 'cmt, 'p> {
    parser: &'p mut Parser<'cmt, 's>,
    original_state: ParserState,
}

impl<'cmt, 's: 'cmt, 'p> Deref for WithState<'cmt, 's, 'p> {
    type Target = Parser<'cmt, 's>;

    fn deref(&self) -> &Self::Target {
        self.parser
    }
}

impl<'cmt, 's: 'cmt, 'p> DerefMut for WithState<'cmt, 's, 'p> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.parser
    }
}

impl<'cmt, 's: 'cmt, 'p> Drop for WithState<'cmt, 's, 'p> {
    fn drop(&mut self) {
        mem::swap(&mut self.parser.state, &mut self.original_state);
    }
}
