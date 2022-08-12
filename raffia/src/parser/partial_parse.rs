use super::Parser;
use crate::ast::*;
use crate::error::PResult;

pub trait Parse<'cmt, 's: 'cmt>: Sized {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self>;
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for AtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_at_rule()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComponentValue<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_component_value()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComponentValues<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_component_values(/* allow_comma */ true)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Declaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_declaration()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaQuery<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_media_query()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediqQueryList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_media_query_list()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for QualifiedRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_qualified_rule()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SelectorList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_selector_list()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SimpleBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_simple_block()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Stylesheet<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_stylesheet()
    }
}
