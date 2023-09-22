//! All kinds of AST nodes are here.

use crate::{pos::Span, tokenizer::TokenWithSpan, util::CowStr};
use raffia_macro::{EnumAsIs, SpanIgnoredEq, Spanned};
#[cfg(feature = "serialize")]
use serde::Serialize;
use smallvec::SmallVec;

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct AnPlusB {
    pub a: i32,
    pub b: i32,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct AtRule<'s> {
    pub name: Ident<'s>,
    pub prelude: Option<AtRulePrelude<'s>>,
    pub block: Option<SimpleBlock<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum AtRulePrelude<'s> {
    Charset(Str<'s>),
    ColorProfile(ColorProfilePrelude<'s>),
    Container(ContainerPrelude<'s>),
    CounterStyle(InterpolableIdent<'s>),
    CustomMedia(Box<CustomMedia<'s>>),
    Document(DocumentPrelude<'s>),
    FontFeatureValues(FontFamilyName<'s>),
    FontPaletteValues(InterpolableIdent<'s>),
    Import(Box<ImportPrelude<'s>>),
    Keyframes(KeyframesName<'s>),
    Layer(LayerName<'s>),
    LessImport(Box<LessImportPrelude<'s>>),
    LessPlugin(Box<LessPlugin<'s>>),
    Media(MediaQueryList<'s>),
    Namespace(Box<NamespacePrelude<'s>>),
    Nest(SelectorList<'s>),
    Page(PageSelectorList<'s>),
    PositionFallback(InterpolableIdent<'s>),
    Property(InterpolableIdent<'s>),
    SassAtRoot(SassAtRoot<'s>),
    SassContent(SassContent<'s>),
    SassEach(Box<SassEach<'s>>),
    SassExpr(Box<ComponentValue<'s>>),
    SassExtend(Box<SassExtend<'s>>),
    SassFor(Box<SassFor<'s>>),
    SassForward(Box<SassForward<'s>>),
    SassFunction(Box<SassFunction<'s>>),
    SassImport(SassImportPrelude<'s>),
    SassInclude(Box<SassInclude<'s>>),
    SassMixin(Box<SassMixin<'s>>),
    SassUse(Box<SassUse<'s>>),
    Scope(Box<ScopePrelude<'s>>),
    ScrollTimeline(InterpolableIdent<'s>),
    Supports(SupportsCondition<'s>),
    Unknown(Box<UnknownAtRulePrelude<'s>>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct AttributeSelector<'s> {
    pub name: WqName<'s>,
    pub matcher: Option<AttributeSelectorMatcher>,
    pub value: Option<AttributeSelectorValue<'s>>,
    pub modifier: Option<AttributeSelectorModifier<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct AttributeSelectorMatcher {
    pub kind: AttributeSelectorMatcherKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum AttributeSelectorMatcherKind {
    /// `=`
    Exact,
    /// `~=`
    MatchWord,
    /// `|=`
    ExactOrPrefixThenHyphen,
    /// `^=`
    Prefix,
    /// `$=`
    Suffix,
    /// `*=`
    Substring,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct AttributeSelectorModifier<'s> {
    pub ident: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum AttributeSelectorValue<'s> {
    Ident(InterpolableIdent<'s>),
    Str(InterpolableStr<'s>),
    Percentage(Percentage<'s>),
    LessEscapedStr(LessEscapedStr<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct BracketBlock<'s> {
    pub value: Vec<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Calc<'s> {
    pub left: Box<ComponentValue<'s>>,
    pub op: CalcOperator,
    pub right: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct CalcOperator {
    pub kind: CalcOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum CalcOperatorKind {
    Plus,
    Minus,
    Multiply,
    Division,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ClassSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ColorProfilePrelude<'s> {
    DashedIdent(InterpolableIdent<'s>),
    DeviceCmyk(Ident<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Combinator {
    pub kind: CombinatorKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ComplexSelector<'s> {
    pub children: SmallVec<[ComplexSelectorChild<'s>; 3]>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ComplexSelectorChild<'s> {
    CompoundSelector(CompoundSelector<'s>),
    Combinator(Combinator),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ComponentValue<'s> {
    BracketBlock(BracketBlock<'s>),
    Calc(Calc<'s>),
    Delimiter(Delimiter),
    Dimension(Dimension<'s>),
    Function(Function<'s>),
    HexColor(HexColor<'s>),
    IdSelector(IdSelector<'s>),
    ImportantAnnotation(ImportantAnnotation<'s>),
    InterpolableIdent(InterpolableIdent<'s>),
    InterpolableStr(InterpolableStr<'s>),
    LayerName(LayerName<'s>),
    LessBinaryOperation(LessBinaryOperation<'s>),
    LessCondition(Box<LessCondition<'s>>),
    LessDetachedRuleset(LessDetachedRuleset<'s>),
    LessEscapedStr(LessEscapedStr<'s>),
    LessJavaScriptSnippet(LessJavaScriptSnippet<'s>),
    LessList(LessList<'s>),
    LessMixinCall(LessMixinCall<'s>),
    LessNamespaceValue(Box<LessNamespaceValue<'s>>),
    LessNegativeValue(LessNegativeValue<'s>),
    LessParenthesizedOperation(LessParenthesizedOperation<'s>),
    LessPercentKeyword(LessPercentKeyword),
    LessPropertyVariable(LessPropertyVariable<'s>),
    LessVariable(LessVariable<'s>),
    LessVariableVariable(LessVariableVariable<'s>),
    Number(Number<'s>),
    Percentage(Percentage<'s>),
    Ratio(Ratio<'s>),
    SassArbitraryArgument(SassArbitraryArgument<'s>),
    SassBinaryExpression(SassBinaryExpression<'s>),
    SassKeywordArgument(SassKeywordArgument<'s>),
    SassList(SassList<'s>),
    SassMap(SassMap<'s>),
    SassQualifiedName(SassQualifiedName<'s>),
    SassNestingDeclaration(SassNestingDeclaration<'s>),
    SassParenthesizedExpression(SassParenthesizedExpression<'s>),
    SassParentSelector(NestingSelector<'s>),
    SassUnaryExpression(SassUnaryExpression<'s>),
    SassVariable(SassVariable<'s>),
    TokenWithSpan(TokenWithSpan<'s>),
    UnicodeRange(UnicodeRange<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ComponentValues<'s> {
    pub values: Vec<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct CompoundSelector<'s> {
    pub children: Vec<SimpleSelector<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct CompoundSelectorList<'s> {
    pub selectors: Vec<CompoundSelector<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ContainerCondition<'s> {
    pub conditions: Vec<ContainerConditionKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ContainerConditionKind<'s> {
    QueryInParens(QueryInParens<'s>),
    And(ContainerConditionAnd<'s>),
    Or(ContainerConditionOr<'s>),
    Not(ContainerConditionNot<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ContainerConditionAnd<'s> {
    pub keyword: Ident<'s>,
    pub query_in_parens: QueryInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ContainerConditionNot<'s> {
    pub keyword: Ident<'s>,
    pub query_in_parens: QueryInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ContainerConditionOr<'s> {
    pub keyword: Ident<'s>,
    pub query_in_parens: QueryInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ContainerPrelude<'s> {
    pub name: Option<InterpolableIdent<'s>>,
    pub condition: ContainerCondition<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct CustomMedia<'s> {
    pub name: InterpolableIdent<'s>,
    pub value: CustomMediaValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum CustomMediaValue<'s> {
    MediaQueryList(MediaQueryList<'s>),
    True(Ident<'s>),
    False(Ident<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Declaration<'s> {
    pub name: InterpolableIdent<'s>,
    pub colon_span: Span,
    pub value: Vec<ComponentValue<'s>>,
    pub important: Option<ImportantAnnotation<'s>>,
    pub less_property_merge: Option<LessPropertyMerge>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Delimiter {
    pub kind: DelimiterKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum DelimiterKind {
    Comma,
    Solidus,
    Semicolon,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Dimension<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub kind: DimensionKind,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum DimensionKind {
    Length,
    Angle,
    Duration,
    Frequency,
    Resolution,
    Flex,
    Unknown,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct DocumentPrelude<'s> {
    pub matchers: Vec<DocumentPreludeMatcher<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum DocumentPreludeMatcher<'s> {
    Url(Url<'s>),
    Function(Function<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum FontFamilyName<'s> {
    Str(InterpolableStr<'s>),
    Unquoted(UnquotedFontFamilyName<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Function<'s> {
    pub name: FunctionName<'s>,
    pub args: Vec<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum FunctionName<'s> {
    Ident(InterpolableIdent<'s>),
    SassQualifiedName(Box<SassQualifiedName<'s>>),
    LessListFunction(LessListFunction),
    LessFormatFunction(LessFormatFunction),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct HexColor<'s> {
    pub value: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Ident<'s> {
    pub name: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ImportPrelude<'s> {
    pub href: ImportPreludeHref<'s>,
    pub layer: Option<ImportPreludeLayer<'s>>,
    pub supports: Option<ImportPreludeSupports<'s>>,
    pub media: Option<MediaQueryList<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ImportPreludeHref<'s> {
    Str(InterpolableStr<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ImportPreludeLayer<'s> {
    Empty(Ident<'s>),
    WithName(Function<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ImportPreludeSupports<'s> {
    pub kind: ImportPreludeSupportsKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ImportPreludeSupportsKind<'s> {
    SupportsCondition(SupportsCondition<'s>),
    Declaration(Declaration<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum InterpolableIdent<'s> {
    Literal(Ident<'s>),
    SassInterpolated(SassInterpolatedIdent<'s>),
    LessInterpolated(LessInterpolatedIdent<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct InterpolableIdentStaticPart<'s> {
    pub value: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum InterpolableStr<'s> {
    Literal(Str<'s>),
    SassInterpolated(SassInterpolatedStr<'s>),
    LessInterpolated(LessInterpolatedStr<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct InterpolableStrStaticPart<'s> {
    pub value: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct InterpolableUrlStaticPart<'s> {
    pub value: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct IdSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ImportantAnnotation<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct KeyframeBlock<'s> {
    pub selectors: Vec<KeyframeSelector<'s>>,
    pub comma_spans: Vec<Span>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum KeyframeSelector<'s> {
    Ident(InterpolableIdent<'s>),
    Percentage(Percentage<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum KeyframesName<'s> {
    Ident(InterpolableIdent<'s>),
    Str(InterpolableStr<'s>),
    LessVariable(LessVariable<'s>),
    LessEscapedStr(LessEscapedStr<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LanguageRange<'s> {
    Str(InterpolableStr<'s>),
    Ident(InterpolableIdent<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LanguageRangeList<'s> {
    pub ranges: Vec<LanguageRange<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LayerName<'s> {
    pub idents: Vec<InterpolableIdent<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessBinaryCondition<'s> {
    pub left: Box<LessCondition<'s>>,
    pub op: LessBinaryConditionOperator,
    pub right: Box<LessCondition<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessBinaryConditionOperator {
    pub kind: LessBinaryConditionOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum LessBinaryConditionOperatorKind {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    EqualOrGreaterThan,
    EqualOrLessThan,
    And,
    Or,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessBinaryOperation<'s> {
    pub left: Box<ComponentValue<'s>>,
    pub op: LessOperationOperator,
    pub right: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessCondition<'s> {
    Binary(LessBinaryCondition<'s>),
    Negated(LessNegatedCondition<'s>),
    Parenthesized(LessParenthesizedCondition<'s>),
    Value(ComponentValue<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessConditionalQualifiedRule<'s> {
    pub selector: SelectorList<'s>,
    pub guard: LessConditions<'s>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessConditions<'s> {
    pub conditions: Vec<LessCondition<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessDetachedRuleset<'s> {
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessEscapedStr<'s> {
    pub str: Str<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessExtend<'s> {
    pub selector: ComplexSelector<'s>,
    pub all: Option<Ident<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessExtendList<'s> {
    pub elements: Vec<LessExtend<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessExtendRule<'s> {
    pub nesting_selector: NestingSelector<'s>,
    pub name_of_extend: Ident<'s>,
    pub extend: LessExtendList<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessFormatFunction {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessImportOptions<'s> {
    pub names: Vec<Ident<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessImportPrelude<'s> {
    pub href: ImportPreludeHref<'s>,
    pub options: LessImportOptions<'s>,
    pub media: Option<MediaQueryList<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessInterpolatedIdent<'s> {
    pub elements: Vec<LessInterpolatedIdentElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessInterpolatedIdentElement<'s> {
    Variable(LessVariableInterpolation<'s>),
    Property(LessPropertyInterpolation<'s>),
    Static(InterpolableIdentStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessInterpolatedStr<'s> {
    pub elements: Vec<LessInterpolatedStrElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessInterpolatedStrElement<'s> {
    Variable(LessVariableInterpolation<'s>),
    Property(LessPropertyInterpolation<'s>),
    Static(InterpolableStrStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessJavaScriptSnippet<'s> {
    pub code: &'s str,
    pub raw: &'s str,
    pub escaped: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessList<'s> {
    pub elements: Vec<ComponentValue<'s>>,
    pub comma_spans: Option<Vec<Span>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessListFunction {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessLookup<'s> {
    pub name: Option<LessLookupName<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessLookupName<'s> {
    LessVariable(LessVariable<'s>),
    LessVariableVariable(LessVariableVariable<'s>),
    LessPropertyVariable(LessPropertyVariable<'s>),
    Ident(Ident<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessLookups<'s> {
    pub lookups: Vec<LessLookup<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessMixinArgument<'s> {
    Named(LessMixinNamedArgument<'s>),
    Value(ComponentValue<'s>),
    Variadic(LessMixinVariadicArgument<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinCall<'s> {
    pub callee: LessMixinCallee<'s>,
    pub args: Option<Vec<LessMixinArgument<'s>>>,
    pub important: Option<ImportantAnnotation<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinCallee<'s> {
    pub children: Vec<LessMixinCalleeChild<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinCalleeChild<'s> {
    pub name: LessMixinName<'s>,
    pub combinator: Option<Combinator>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinDefinition<'s> {
    pub name: LessMixinName<'s>,
    pub params: Vec<LessMixinParameter<'s>>,
    pub guard: Option<LessConditions<'s>>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessMixinName<'s> {
    ClassSelector(ClassSelector<'s>),
    IdSelector(IdSelector<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinNamedArgument<'s> {
    pub name: LessMixinParameterName<'s>,
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinNamedParameter<'s> {
    pub name: LessMixinParameterName<'s>,
    pub value: Option<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessMixinParameter<'s> {
    Named(LessMixinNamedParameter<'s>),
    Unnamed(LessMixinUnnamedParameter<'s>),
    Variadic(LessMixinVariadicParameter<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessMixinParameterName<'s> {
    Variable(LessVariable<'s>),
    PropertyVariable(LessPropertyVariable<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinUnnamedParameter<'s> {
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinVariadicArgument<'s> {
    pub name: LessMixinParameterName<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessMixinVariadicParameter<'s> {
    pub name: Option<LessMixinParameterName<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessNamespaceValue<'s> {
    pub callee: LessNamespaceValueCallee<'s>,
    pub lookups: LessLookups<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessNamespaceValueCallee<'s> {
    LessMixinCall(LessMixinCall<'s>),
    LessVariable(LessVariable<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessNegatedCondition<'s> {
    pub condition: Box<LessCondition<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessNegativeValue<'s> {
    pub value: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessOperationOperator {
    pub kind: LessOperationOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum LessOperationOperatorKind {
    Multiply,
    Division,
    Plus,
    Minus,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessParenthesizedCondition<'s> {
    pub condition: Box<LessCondition<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessParenthesizedOperation<'s> {
    pub operation: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessPercentKeyword {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessPlugin<'s> {
    pub path: LessPluginPath<'s>,
    pub args: Option<TokenSeq<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum LessPluginPath<'s> {
    Str(Str<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessPropertyInterpolation<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessPropertyMerge {
    pub kind: LessPropertyMergeKind,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum LessPropertyMergeKind {
    Comma,
    Space,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessPropertyVariable<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessVariable<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessVariableCall<'s> {
    pub variable: LessVariable<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessVariableDeclaration<'s> {
    pub name: LessVariable<'s>,
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessVariableInterpolation<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LessVariableVariable<'s> {
    pub variable: LessVariable<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaAnd<'s> {
    pub keyword: Ident<'s>,
    pub media_in_parens: MediaInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaCondition<'s> {
    pub conditions: Vec<MediaConditionKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaConditionAfterMediaType<'s> {
    pub and: Ident<'s>,
    pub condition: MediaCondition<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum MediaConditionKind<'s> {
    MediaInParens(MediaInParens<'s>),
    And(MediaAnd<'s>),
    Or(MediaOr<'s>),
    Not(MediaNot<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum MediaFeature<'s> {
    Plain(MediaFeaturePlain<'s>),
    Boolean(MediaFeatureBoolean<'s>),
    Range(MediaFeatureRange<'s>),
    RangeInterval(MediaFeatureRangeInterval<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaFeatureComparison {
    pub kind: MediaFeatureComparisonKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum MediaFeatureComparisonKind {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum MediaFeatureName<'s> {
    Ident(InterpolableIdent<'s>),
    SassVariable(SassVariable<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaFeatureBoolean<'s> {
    pub name: MediaFeatureName<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaFeaturePlain<'s> {
    pub name: MediaFeatureName<'s>,
    pub colon_span: Span,
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaFeatureRange<'s> {
    pub left: ComponentValue<'s>,
    pub comparison: MediaFeatureComparison,
    pub right: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaFeatureRangeInterval<'s> {
    pub left: ComponentValue<'s>,
    pub left_comparison: MediaFeatureComparison,
    pub name: MediaFeatureName<'s>,
    pub right_comparison: MediaFeatureComparison,
    pub right: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaInParens<'s> {
    pub kind: MediaInParensKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum MediaInParensKind<'s> {
    MediaCondition(MediaCondition<'s>),
    MediaFeature(Box<MediaFeature<'s>>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaNot<'s> {
    pub keyword: Ident<'s>,
    pub media_in_parens: MediaInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaOr<'s> {
    pub keyword: Ident<'s>,
    pub media_in_parens: MediaInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum MediaQuery<'s> {
    ConditionOnly(MediaCondition<'s>),
    WithType(MediaQueryWithType<'s>),
    LessVariable(LessVariable<'s>),
    LessNamespaceValue(Box<LessNamespaceValue<'s>>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaQueryList<'s> {
    pub queries: Vec<MediaQuery<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct MediaQueryWithType<'s> {
    pub modifier: Option<Ident<'s>>,
    pub media_type: InterpolableIdent<'s>,
    pub condition: Option<MediaConditionAfterMediaType<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct NamespacePrelude<'s> {
    pub prefix: Option<InterpolableIdent<'s>>,
    pub uri: NamespacePreludeUri<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum NamespacePreludeUri<'s> {
    Str(InterpolableStr<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct NestingSelector<'s> {
    pub suffix: Option<InterpolableIdent<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct NsPrefix<'s> {
    pub kind: Option<NsPrefixKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum NsPrefixKind<'s> {
    Ident(InterpolableIdent<'s>),
    Universal(NsPrefixUniversal),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct NsPrefixUniversal {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Nth<'s> {
    pub index: NthIndex<'s>,
    pub matcher: Option<NthMatcher<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum NthIndex<'s> {
    Odd(Ident<'s>),
    Even(Ident<'s>),
    Integer(Number<'s>),
    AnPlusB(AnPlusB),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct NthMatcher<'s> {
    pub selector: Option<SelectorList<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Number<'s> {
    pub value: f32,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct PageSelector<'s> {
    pub name: Option<InterpolableIdent<'s>>,
    pub pseudo: Vec<PseudoPage<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct PageSelectorList<'s> {
    pub selectors: Vec<PageSelector<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Percentage<'s> {
    pub value: Number<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct PseudoClassSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub arg: Option<PseudoClassSelectorArg<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum PseudoClassSelectorArg<'s> {
    CompoundSelector(CompoundSelector<'s>),
    CompoundSelectorList(CompoundSelectorList<'s>),
    Ident(InterpolableIdent<'s>),
    LanguageRangeList(LanguageRangeList<'s>),
    Nth(Nth<'s>),
    Number(Number<'s>),
    RelativeSelectorList(RelativeSelectorList<'s>),
    SelectorList(SelectorList<'s>),
    LessExtendList(LessExtendList<'s>),
    TokenSeq(TokenSeq<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct PseudoElementSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub arg: Option<PseudoElementSelectorArg<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum PseudoElementSelectorArg<'s> {
    CompoundSelector(CompoundSelector<'s>),
    Ident(InterpolableIdent<'s>),
    TokenSeq(TokenSeq<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct PseudoPage<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct QualifiedRule<'s> {
    pub selector: SelectorList<'s>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct QueryInParens<'s> {
    pub kind: QueryInParensKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum QueryInParensKind<'s> {
    ContainerCondition(ContainerCondition<'s>),
    SizeFeature(Box<MediaFeature<'s>>),
    StyleQuery(StyleQuery<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Ratio<'s> {
    pub numerator: Number<'s>,
    pub solidus_span: Span,
    pub denominator: Number<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct RelativeSelector<'s> {
    pub combinator: Option<Combinator>,
    pub complex_selector: ComplexSelector<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct RelativeSelectorList<'s> {
    pub selectors: Vec<RelativeSelector<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassArbitraryArgument<'s> {
    pub value: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassArbitraryParameter<'s> {
    pub name: SassVariable<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassAtRoot<'s> {
    pub kind: SassAtRootKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassAtRootKind<'s> {
    Selector(SelectorList<'s>),
    Query(SassAtRootQuery<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassAtRootQuery<'s> {
    pub modifier: SassAtRootQueryModifier,
    pub colon_span: Span,
    /// space-separated rule names
    pub rules: Vec<SassAtRootQueryRule<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassAtRootQueryModifier {
    pub kind: SassAtRootQueryModifierKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SassAtRootQueryModifierKind {
    With,
    Without,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassAtRootQueryRule<'s> {
    Ident(InterpolableIdent<'s>),
    Str(InterpolableStr<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassBinaryExpression<'s> {
    pub left: Box<ComponentValue<'s>>,
    pub op: SassBinaryOperator,
    pub right: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassBinaryOperator {
    pub kind: SassBinaryOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SassBinaryOperatorKind {
    Multiply,
    Division,
    Modulo,
    Plus,
    Minus,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    EqualsEquals,
    ExclamationEquals,
    And,
    Or,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassConditionalClause<'s> {
    pub condition: ComponentValue<'s>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassContent<'s> {
    pub args: Vec<ComponentValue<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassEach<'s> {
    pub bindings: Vec<SassVariable<'s>>,
    pub comma_spans: Vec<Span>,
    pub in_span: Span,
    pub expr: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassExtend<'s> {
    pub selectors: CompoundSelectorList<'s>,
    pub optional: Option<SassFlag<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassFlag<'s> {
    pub keyword: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassFor<'s> {
    pub binding: SassVariable<'s>,
    pub from_span: Span,
    pub start: ComponentValue<'s>,
    pub end: ComponentValue<'s>,
    pub boundary: SassForBoundary,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassForBoundary {
    pub kind: SassForBoundaryKind,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SassForBoundaryKind {
    Inclusive,
    Exclusive,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassForward<'s> {
    pub path: InterpolableStr<'s>,
    pub prefix: Option<SassForwardPrefix<'s>>,
    pub visibility: Option<SassForwardVisibility<'s>>,
    pub config: Option<SassModuleConfig<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassForwardMember<'s> {
    Ident(Ident<'s>),
    Variable(SassVariable<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassForwardPrefix<'s> {
    pub as_span: Span,
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassForwardVisibility<'s> {
    pub modifier: SassForwardVisibilityModifier,
    pub members: Vec<SassForwardMember<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassForwardVisibilityModifier {
    pub kind: SassForwardVisibilityModifierKind,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SassForwardVisibilityModifierKind {
    Hide,
    Show,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassFunction<'s> {
    pub name: Ident<'s>,
    pub parameters: SassParameters<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassIfAtRule<'s> {
    pub if_clause: SassConditionalClause<'s>,
    pub else_if_clauses: Vec<SassConditionalClause<'s>>,
    pub else_clause: Option<SimpleBlock<'s>>,
    pub else_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassImportPrelude<'s> {
    pub paths: Vec<Str<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassInclude<'s> {
    pub name: FunctionName<'s>,
    pub arguments: Option<SassIncludeArgs<'s>>,
    pub content_block_params: Option<SassIncludeContentBlockParams<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassIncludeArgs<'s> {
    pub args: Vec<ComponentValue<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassIncludeContentBlockParams<'s> {
    pub using_span: Span,
    pub params: SassParameters<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassInterpolatedIdent<'s> {
    pub elements: Vec<SassInterpolatedIdentElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassInterpolatedIdentElement<'s> {
    Expression(ComponentValue<'s>),
    Static(InterpolableIdentStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassInterpolatedStr<'s> {
    pub elements: Vec<SassInterpolatedStrElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassInterpolatedStrElement<'s> {
    Expression(ComponentValue<'s>),
    Static(InterpolableStrStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassInterpolatedUrl<'s> {
    pub elements: Vec<SassInterpolatedUrlElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassInterpolatedUrlElement<'s> {
    Expression(ComponentValue<'s>),
    Static(InterpolableUrlStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassKeywordArgument<'s> {
    pub name: SassVariable<'s>,
    pub colon_span: Span,
    pub value: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassList<'s> {
    pub elements: Vec<ComponentValue<'s>>,
    pub comma_spans: Option<Vec<Span>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassMap<'s> {
    pub items: Vec<SassMapItem<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassMapItem<'s> {
    pub key: ComponentValue<'s>,
    pub colon_span: Span,
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassMixin<'s> {
    pub name: Ident<'s>,
    pub parameters: Option<SassParameters<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassModuleConfig<'s> {
    pub with_span: Span,
    pub lparen_span: Span,
    pub items: Vec<SassModuleConfigItem<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassModuleConfigItem<'s> {
    pub variable: SassVariable<'s>,
    pub colon_span: Span,
    pub value: ComponentValue<'s>,
    pub flags: Vec<SassFlag<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassModuleMemberName<'s> {
    Ident(Ident<'s>),
    Variable(SassVariable<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassNestingDeclaration<'s> {
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassParameter<'s> {
    pub name: SassVariable<'s>,
    pub default_value: Option<SassParameterDefaultValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassParameterDefaultValue<'s> {
    pub colon_span: Span,
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassParameters<'s> {
    pub params: Vec<SassParameter<'s>>,
    pub arbitrary_param: Option<SassArbitraryParameter<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassParenthesizedExpression<'s> {
    pub expr: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassPlaceholderSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassQualifiedName<'s> {
    pub module: Ident<'s>,
    pub member: SassModuleMemberName<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassUnaryExpression<'s> {
    pub op: SassUnaryOperator,
    pub expr: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassUnaryOperator {
    pub kind: SassUnaryOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SassUnaryOperatorKind {
    Plus,
    Minus,
    Not,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassUnnamedNamespace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassUse<'s> {
    pub path: InterpolableStr<'s>,
    pub namespace: Option<SassUseNamespace<'s>>,
    pub config: Option<SassModuleConfig<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassUseNamespace<'s> {
    pub as_span: Span,
    pub kind: SassUseNamespaceKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SassUseNamespaceKind<'s> {
    Named(Ident<'s>),
    Unnamed(SassUnnamedNamespace),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassVariable<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SassVariableDeclaration<'s> {
    pub namespace: Option<Ident<'s>>,
    pub name: SassVariable<'s>,
    pub colon_span: Span,
    pub value: ComponentValue<'s>,
    pub flags: Vec<SassFlag<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ScopeEnd<'s> {
    pub to_span: Span,
    pub lparen_span: Span,
    pub selector: SelectorList<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum ScopePrelude<'s> {
    StartOnly(ScopeStart<'s>),
    EndOnly(ScopeEnd<'s>),
    Both(ScopeStartWithEnd<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ScopeStart<'s> {
    pub selector: SelectorList<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct ScopeStartWithEnd<'s> {
    pub start: ScopeStart<'s>,
    pub end: ScopeEnd<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SelectorList<'s> {
    pub selectors: Vec<ComplexSelector<'s>>,
    pub comma_spans: Vec<Span>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SimpleBlock<'s> {
    pub statements: Vec<Statement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SimpleSelector<'s> {
    Class(ClassSelector<'s>),
    Id(IdSelector<'s>),
    Type(TypeSelector<'s>),
    Attribute(AttributeSelector<'s>),
    PseudoClass(PseudoClassSelector<'s>),
    PseudoElement(PseudoElementSelector<'s>),
    Nesting(NestingSelector<'s>),
    SassPlaceholder(SassPlaceholderSelector<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum Statement<'s> {
    AtRule(AtRule<'s>),
    Declaration(Declaration<'s>),
    KeyframeBlock(KeyframeBlock<'s>),
    LessConditionalQualifiedRule(LessConditionalQualifiedRule<'s>),
    LessExtendRule(LessExtendRule<'s>),
    LessFunctionCall(Function<'s>),
    LessMixinCall(LessMixinCall<'s>),
    LessMixinDefinition(LessMixinDefinition<'s>),
    LessVariableCall(LessVariableCall<'s>),
    LessVariableDeclaration(LessVariableDeclaration<'s>),
    QualifiedRule(QualifiedRule<'s>),
    SassIfAtRule(SassIfAtRule<'s>),
    SassVariableDeclaration(SassVariableDeclaration<'s>),
    UnknownSassAtRule(Box<UnknownSassAtRule<'s>>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Str<'s> {
    pub value: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct StyleCondition<'s> {
    pub conditions: Vec<StyleConditionKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum StyleConditionKind<'s> {
    StyleInParens(StyleInParens<'s>),
    And(StyleConditionAnd<'s>),
    Or(StyleConditionOr<'s>),
    Not(StyleConditionNot<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct StyleConditionAnd<'s> {
    pub keyword: Ident<'s>,
    pub style_in_parens: StyleInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct StyleConditionNot<'s> {
    pub keyword: Ident<'s>,
    pub style_in_parens: StyleInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct StyleConditionOr<'s> {
    pub keyword: Ident<'s>,
    pub style_in_parens: StyleInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct StyleInParens<'s> {
    pub kind: StyleInParensKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum StyleInParensKind<'s> {
    Condition(StyleCondition<'s>),
    Feature(Declaration<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum StyleQuery<'s> {
    Condition(StyleCondition<'s>),
    Feature(Declaration<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Stylesheet<'s> {
    pub statements: Vec<Statement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SupportsAnd<'s> {
    pub keyword: Ident<'s>,
    pub condition: SupportsInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SupportsCondition<'s> {
    pub conditions: Vec<SupportsConditionKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SupportsConditionKind<'s> {
    Not(SupportsNot<'s>),
    And(SupportsAnd<'s>),
    Or(SupportsOr<'s>),
    SupportsInParens(SupportsInParens<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SupportsDecl<'s> {
    pub decl: Declaration<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SupportsInParens<'s> {
    pub kind: SupportsInParensKind<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum SupportsInParensKind<'s> {
    SupportsCondition(SupportsCondition<'s>),
    Feature(Box<SupportsDecl<'s>>),
    Selector(SelectorList<'s>),
    Function(Function<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SupportsNot<'s> {
    pub keyword: Ident<'s>,
    pub condition: SupportsInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct SupportsOr<'s> {
    pub keyword: Ident<'s>,
    pub condition: SupportsInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct TagNameSelector<'s> {
    pub name: WqName<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct TokenSeq<'s> {
    pub tokens: Vec<TokenWithSpan<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum TypeSelector<'s> {
    TagName(TagNameSelector<'s>),
    Universal(UniversalSelector<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct UnicodeRange<'s> {
    pub prefix: char,
    pub start: u32,
    pub start_raw: &'s str,
    pub end: u32,
    pub end_raw: Option<&'s str>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct UniversalSelector<'s> {
    pub prefix: Option<NsPrefix<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum UnknownAtRulePrelude<'s> {
    ComponentValue(ComponentValue<'s>),
    TokenSeq(TokenSeq<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct UnknownSassAtRule<'s> {
    pub name: InterpolableIdent<'s>,
    pub prelude: Option<UnknownAtRulePrelude<'s>>,
    pub block: Option<SimpleBlock<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct UnquotedFontFamilyName<'s> {
    pub idents: Vec<InterpolableIdent<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct Url<'s> {
    pub name: Ident<'s>,
    pub value: Option<UrlValue<'s>>,
    pub modifiers: Vec<UrlModifier<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum UrlModifier<'s> {
    Ident(InterpolableIdent<'s>),
    Function(Function<'s>),
}

/// `)` is excluded
#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct UrlRaw<'s> {
    pub value: CowStr<'s>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum UrlValue<'s> {
    Raw(UrlRaw<'s>),
    SassInterpolated(SassInterpolatedUrl<'s>),
    Str(InterpolableStr<'s>),
    LessEscapedStr(LessEscapedStr<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct WqName<'s> {
    pub name: InterpolableIdent<'s>,
    pub prefix: Option<NsPrefix<'s>>,
    pub span: Span,
}
