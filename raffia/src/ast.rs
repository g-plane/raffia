use crate::pos::Span;
use raffia_derive::Spanned;
use std::borrow::Cow;

#[derive(Clone, Debug, Spanned)]
pub struct Angle<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AttributeSelector<'a> {
    pub name: WqName<'a>,
    pub matcher: Option<AttributeSelectorMatcher>,
    pub value: Option<AttributeSelectorValue<'a>>,
    pub modifier: Option<AttributeSelectorModifier<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AttributeSelectorMatcher {
    pub kind: AttributeSelectorMatcherKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum AttributeSelectorMatcherKind {
    Equals,
    Tilde,
    Bar,
    Caret,
    Dollar,
    Asterisk,
}

#[derive(Clone, Debug, Spanned)]
pub struct AttributeSelectorModifier<'a> {
    pub ident: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum AttributeSelectorValue<'a> {
    Ident(Ident<'a>),
    Str(Str<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct ClassSelector<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Combinator {
    pub kind: CombinatorKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum CombinatorKind {
    /// ` `
    Descendant,
    /// `+`
    NextSibling,
    /// `>`
    Child,
    /// `~`
    LaterSibling,
    /// `||`
    Column,
}

#[derive(Clone, Debug, Spanned)]
pub struct ComplexSelector<'a> {
    pub children: Vec<ComplexSelectorChild<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum ComplexSelectorChild<'a> {
    CompoundSelector(CompoundSelector<'a>),
    Combinator(Combinator),
}

#[derive(Clone, Debug, Spanned)]
pub enum ComponentValue<'a> {
    Delimiter(Delimiter),
    Dimension(Dimension<'a>),
    Function(Function<'a>),
    HexColor(HexColor<'a>),
    InterpolableIdent(InterpolableIdent<'a>),
    Number(Number<'a>),
    Percentage(Percentage<'a>),
    SassVariable(SassVariable<'a>),
    Str(Str<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct ComponentValues<'a> {
    pub values: Vec<ComponentValue<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct CompoundSelector<'a> {
    pub children: Vec<SimpleSelector<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Declaration<'a> {
    pub name: InterpolableIdent<'a>,
    pub value: ComponentValues<'a>,
    pub less_property_merge: Option<LessPropertyMerge>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Delimiter {
    pub kind: DelimiterKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum DelimiterKind {
    Comma,
    Solidus,
    Semicolon,
}

#[derive(Clone, Debug, Spanned)]
pub enum Dimension<'a> {
    Length(Length<'a>),
    Angle(Angle<'a>),
    Duration(Duration<'a>),
    Frequency(Frequency<'a>),
    Resolution(Resolution<'a>),
    Flex(Flex<'a>),
    Unknown(UnknownDimension<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Duration<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Flex<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub args: Vec<ComponentValue<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Frequency<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct HexColor<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Ident<'a> {
    pub name: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum InterpolableIdent<'a> {
    Literal(Ident<'a>),
    SassInterpolated(SassInterpolatedIdent<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct InterpolableIdentLiteralPart<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct IdSelector<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessPropertyMerge {
    pub kind: LessPropertyMergeKind,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum LessPropertyMergeKind {
    Comma,
    Space,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessVariableDeclaration<'a> {
    pub name: Ident<'a>,
    pub value: ComponentValues<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Length<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct NestingSelector {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct NsPrefix<'a> {
    pub kind: Option<NsPrefixKind<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum NsPrefixKind<'a> {
    Ident(Ident<'a>),
    Universal(NsPrefixUniversal),
}

#[derive(Clone, Debug, Spanned)]
pub struct NsPrefixUniversal {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Number<'a> {
    pub value: f64,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Percentage<'a> {
    pub value: Number<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct QualifiedRule<'a> {
    pub selector: SelectorList<'a>,
    pub block: SimpleBlock<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Resolution<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassInterpolatedIdent<'a> {
    pub elements: Vec<SassInterpolatedIdentElement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SassInterpolatedIdentElement<'a> {
    Expression(ComponentValues<'a>),
    Literal(InterpolableIdentLiteralPart<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SassVariable<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassVariableDeclaration<'a> {
    pub name: SassVariable<'a>,
    pub value: ComponentValues<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SelectorList<'a> {
    pub selectors: Vec<ComplexSelector<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SimpleBlock<'a> {
    pub elements: Vec<SimpleBlockElement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SimpleBlockElement<'a> {
    Declaration(Declaration<'a>),
    LessVariableDeclaration(LessVariableDeclaration<'a>),
    QualifiedRule(QualifiedRule<'a>),
    SassVariableDeclaration(SassVariableDeclaration<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub enum SimpleSelector<'a> {
    Class(ClassSelector<'a>),
    Id(IdSelector<'a>),
    Type(TypeSelector<'a>),
    Attribute(AttributeSelector<'a>),
    Nesting(NestingSelector),
}

#[derive(Clone, Debug, Spanned)]
pub struct Str<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Stylesheet<'a> {
    pub statements: Vec<TopLevelStatement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct TagNameSelector<'a> {
    pub name: WqName<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum TopLevelStatement<'a> {
    LessVariableDeclaration(LessVariableDeclaration<'a>),
    QualifiedRule(QualifiedRule<'a>),
    SassVariableDeclaration(SassVariableDeclaration<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub enum TypeSelector<'a> {
    TagName(TagNameSelector<'a>),
    Universal(UniversalSelector<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct UniversalSelector<'a> {
    pub prefix: Option<NsPrefix<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct UnknownDimension<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct WqName<'a> {
    pub name: Ident<'a>,
    pub prefix: Option<NsPrefix<'a>>,
    pub span: Span,
}
