use crate::pos::Span;
use raffia_derive::Spanned;
use std::borrow::Cow;

#[derive(Clone, Debug, Spanned)]
pub struct Angle<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AtRule<'s> {
    pub name: Ident<'s>,
    pub prelude: Option<AtRulePrelude<'s>>,
    pub block: Option<SimpleBlock<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum AtRulePrelude<'s> {
    Charset(Str<'s>),
    ColorProfile(ColorProfilePrelude<'s>),
    CounterStyle(InterpolableIdent<'s>),
    CustomMedia(CustomMedia<'s>),
    Document(DocumentPrelude<'s>),
    FontFeatureValues(FontFamilyName<'s>),
    FontPaletteValues(InterpolableIdent<'s>),
    Import(ImportPrelude<'s>),
    Keyframes(KeyframesName<'s>),
    Layer(LayerName<'s>),
    Media(MediqQueryList<'s>),
    Namespace(NamespacePrelude<'s>),
    Nest(SelectorList<'s>),
    PositionFallback(InterpolableIdent<'s>),
    Property(InterpolableIdent<'s>),
    ScrollTimeline(InterpolableIdent<'s>),
    Supports(SupportsCondition<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct AttributeSelector<'s> {
    pub name: WqName<'s>,
    pub matcher: Option<AttributeSelectorMatcher>,
    pub value: Option<AttributeSelectorValue<'s>>,
    pub modifier: Option<AttributeSelectorModifier<'s>>,
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
pub struct AttributeSelectorModifier<'s> {
    pub ident: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum AttributeSelectorValue<'s> {
    Ident(InterpolableIdent<'s>),
    Str(InterpolableStr<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum BinaryOperatorKind {
    Multiply,
    Modulo,
    Plus,
    Minus,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    EqualsEquals,
    ExclamationEquals,
    And,
    Or,
}

#[derive(Clone, Debug, Spanned)]
pub struct ClassSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum ColorProfilePrelude<'s> {
    DashedIdent(InterpolableIdent<'s>),
    DeviceCmyk(Ident<'s>),
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
pub struct ComplexSelector<'s> {
    pub children: Vec<ComplexSelectorChild<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum ComplexSelectorChild<'s> {
    CompoundSelector(CompoundSelector<'s>),
    Combinator(Combinator),
}

#[derive(Clone, Debug, Spanned)]
pub enum ComponentValue<'s> {
    Delimiter(Delimiter),
    Dimension(Dimension<'s>),
    Function(Function<'s>),
    HexColor(HexColor<'s>),
    InterpolableIdent(InterpolableIdent<'s>),
    InterpolableStr(InterpolableStr<'s>),
    LayerName(LayerName<'s>),
    LessVariable(LessVariable<'s>),
    Number(Number<'s>),
    Percentage(Percentage<'s>),
    Ratio(Ratio<'s>),
    SassBinaryExpression(SassBinaryExpression<'s>),
    SassParenthesizedExpression(SassParenthesizedExpression<'s>),
    SassUnaryExpression(SassUnaryExpression<'s>),
    SassVariable(SassVariable<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct ComponentValues<'s> {
    pub values: Vec<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct CompoundSelector<'s> {
    pub children: Vec<SimpleSelector<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct CustomMedia<'s> {
    pub name: InterpolableIdent<'s>,
    pub value: CustomMediaValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum CustomMediaValue<'s> {
    MediaQueryList(MediqQueryList<'s>),
    True(Ident<'s>),
    False(Ident<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Declaration<'s> {
    pub name: InterpolableIdent<'s>,
    pub value: ComponentValues<'s>,
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
pub enum Dimension<'s> {
    Length(Length<'s>),
    Angle(Angle<'s>),
    Duration(Duration<'s>),
    Frequency(Frequency<'s>),
    Resolution(Resolution<'s>),
    Flex(Flex<'s>),
    Unknown(UnknownDimension<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct DocumentPrelude<'s> {
    pub matchers: Vec<DocumentPreludeMatcher<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum DocumentPreludeMatcher<'s> {
    Url(Url<'s>),
    Function(Function<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Duration<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Flex<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum FontFamilyName<'s> {
    Str(InterpolableStr<'s>),
    Unquoted(UnquotedFontFamilyName<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Function<'s> {
    pub name: InterpolableIdent<'s>,
    pub args: Vec<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Frequency<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct HexColor<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Ident<'s> {
    pub name: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct ImportPrelude<'s> {
    pub href: ImportPreludeHref<'s>,
    pub layer: Option<ImportPreludeLayer<'s>>,
    pub supports: Option<ImportPreludeSupports<'s>>,
    pub media: Option<MediqQueryList<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum ImportPreludeHref<'s> {
    Str(InterpolableStr<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub enum ImportPreludeLayer<'s> {
    Empty(Ident<'s>),
    WithName(Function<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub enum ImportPreludeSupports<'s> {
    SupportsCondition(SupportsCondition<'s>),
    Declaration(Declaration<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub enum InterpolableIdent<'s> {
    Literal(Ident<'s>),
    SassInterpolated(SassInterpolatedIdent<'s>),
    LessInterpolated(LessInterpolatedIdent<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct InterpolableIdentStaticPart<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum InterpolableStr<'s> {
    Literal(Str<'s>),
    SassInterpolated(SassInterpolatedStr<'s>),
    LessInterpolated(LessInterpolatedStr<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct InterpolableStrStaticPart<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct InterpolableUrlStaticPart<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct IdSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct KeyframeBlock<'s> {
    pub prelude: Vec<KeyframeSelector<'s>>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum KeyframeSelector<'s> {
    Ident(InterpolableIdent<'s>),
    Percentage(Percentage<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub enum KeyframesName<'s> {
    Ident(InterpolableIdent<'s>),
    Str(InterpolableStr<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct LayerName<'s> {
    pub idents: Vec<InterpolableIdent<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessInterpolatedIdent<'s> {
    pub elements: Vec<LessInterpolatedIdentElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum LessInterpolatedIdentElement<'s> {
    Variable(LessVariableInterpolation<'s>),
    Static(InterpolableIdentStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct LessInterpolatedStr<'s> {
    pub elements: Vec<LessInterpolatedStrElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum LessInterpolatedStrElement<'s> {
    Variable(LessVariableInterpolation<'s>),
    Static(InterpolableStrStaticPart<'s>),
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
pub struct LessVariable<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessVariableDeclaration<'s> {
    pub name: LessVariable<'s>,
    pub value: ComponentValues<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessVariableInterpolation<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Length<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaAnd<'s> {
    pub ident: Ident<'s>,
    pub media_in_parens: MediaInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaCondition<'s> {
    pub conditions: Vec<MediaConditionKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaConditionKind<'s> {
    MediaInParens(MediaInParens<'s>),
    And(MediaAnd<'s>),
    Or(MediaOr<'s>),
    Not(MediaNot<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaFeature<'s> {
    Plain(MediaFeaturePlain<'s>),
    Boolean(MediaFeatureBoolean<'s>),
    Range(MediaFeatureRange<'s>),
    RangeInterval(MediaFeatureRangeInterval<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureComparison {
    pub kind: MediaFeatureComparisonKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MediaFeatureComparisonKind {
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaFeatureName<'s> {
    Ident(InterpolableIdent<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureBoolean<'s> {
    pub name: MediaFeatureName<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeaturePlain<'s> {
    pub name: MediaFeatureName<'s>,
    pub value: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureRange<'s> {
    pub left: ComponentValue<'s>,
    pub comparison: MediaFeatureComparison,
    pub right: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureRangeInterval<'s> {
    pub left: ComponentValue<'s>,
    pub left_comparison: MediaFeatureComparison,
    pub name: MediaFeatureName<'s>,
    pub right_comparison: MediaFeatureComparison,
    pub right: ComponentValue<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaInParens<'s> {
    MediaCondition(MediaCondition<'s>),
    MediaFeature(Box<MediaFeature<'s>>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaNot<'s> {
    pub ident: Ident<'s>,
    pub media_in_parens: MediaInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaOr<'s> {
    pub ident: Ident<'s>,
    pub media_in_parens: MediaInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaQuery<'s> {
    ConditionOnly(MediaCondition<'s>),
    WithType(MediaQueryWithType<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediqQueryList<'s> {
    pub queries: Vec<MediaQuery<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaQueryWithType<'s> {
    pub modifier: Option<Ident<'s>>,
    pub media_type: InterpolableIdent<'s>,
    pub condition: Option<MediaCondition<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct NamespacePrelude<'s> {
    pub prefix: Option<InterpolableIdent<'s>>,
    pub uri: NamespacePreludeUri<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum NamespacePreludeUri<'s> {
    Str(InterpolableStr<'s>),
    Url(Url<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct NestingSelector {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct NsPrefix<'s> {
    pub kind: Option<NsPrefixKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum NsPrefixKind<'s> {
    Ident(InterpolableIdent<'s>),
    Universal(NsPrefixUniversal),
}

#[derive(Clone, Debug, Spanned)]
pub struct NsPrefixUniversal {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Number<'s> {
    pub value: f64,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Percentage<'s> {
    pub value: Number<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct QualifiedRule<'s> {
    pub selector: SelectorList<'s>,
    pub block: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Ratio<'s> {
    pub numerator: Number<'s>,
    pub denominator: Number<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Resolution<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassBinaryExpression<'s> {
    pub left: Box<ComponentValue<'s>>,
    pub op: BinaryOperator,
    pub right: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassDebugAtRule<'s> {
    pub expr: ComponentValues<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassEachAtRule<'s> {
    pub bindings: Vec<SassVariable<'s>>,
    pub expr: ComponentValue<'s>,
    pub body: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassErrorAtRule<'s> {
    pub expr: ComponentValues<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassForAtRule<'s> {
    pub binding: SassVariable<'s>,
    pub start: ComponentValue<'s>,
    pub end: ComponentValue<'s>,
    pub is_exclusive: bool,
    pub body: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassInterpolatedIdent<'s> {
    pub elements: Vec<SassInterpolatedIdentElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SassInterpolatedIdentElement<'s> {
    Expression(ComponentValues<'s>),
    Static(InterpolableIdentStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SassInterpolatedStr<'s> {
    pub elements: Vec<SassInterpolatedStrElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SassInterpolatedStrElement<'s> {
    Expression(ComponentValues<'s>),
    Static(InterpolableStrStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SassInterpolatedUrl<'s> {
    pub elements: Vec<SassInterpolatedUrlElement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SassInterpolatedUrlElement<'s> {
    Expression(ComponentValues<'s>),
    Static(InterpolableUrlStaticPart<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SassParenthesizedExpression<'s> {
    pub expr: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassPlaceholderSelector<'s> {
    pub name: InterpolableIdent<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassUnaryExpression<'s> {
    pub op: SassUnaryOperator,
    pub expr: Box<ComponentValue<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassUnaryOperator {
    pub kind: SassUnaryOperatorKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum SassUnaryOperatorKind {
    Plus,
    Minus,
    Not,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassVariable<'s> {
    pub name: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassVariableDeclaration<'s> {
    pub name: SassVariable<'s>,
    pub value: ComponentValues<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassWarnAtRule<'s> {
    pub expr: ComponentValues<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassWhileAtRule<'s> {
    pub condition: ComponentValue<'s>,
    pub body: SimpleBlock<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SelectorList<'s> {
    pub selectors: Vec<ComplexSelector<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SimpleBlock<'s> {
    pub statements: Vec<Statement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SimpleSelector<'s> {
    Class(ClassSelector<'s>),
    Id(IdSelector<'s>),
    Type(TypeSelector<'s>),
    Attribute(AttributeSelector<'s>),
    Nesting(NestingSelector),
    SassPlaceholder(SassPlaceholderSelector<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub enum Statement<'s> {
    AtRule(AtRule<'s>),
    Declaration(Declaration<'s>),
    KeyframeBlock(KeyframeBlock<'s>),
    LessVariableDeclaration(LessVariableDeclaration<'s>),
    QualifiedRule(QualifiedRule<'s>),
    SassDebugAtRule(SassDebugAtRule<'s>),
    SassEachAtRule(SassEachAtRule<'s>),
    SassErrorAtRule(SassErrorAtRule<'s>),
    SassForAtRule(SassForAtRule<'s>),
    SassVariableDeclaration(SassVariableDeclaration<'s>),
    SassWarnAtRule(SassWarnAtRule<'s>),
    SassWhileAtRule(SassWhileAtRule<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Str<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Stylesheet<'s> {
    pub statements: Vec<Statement<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsAnd<'s> {
    pub ident: Ident<'s>,
    pub condition: SupportsInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsCondition<'s> {
    pub conditions: Vec<SupportsConditionKind<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SupportsConditionKind<'s> {
    Not(SupportsNot<'s>),
    And(SupportsAnd<'s>),
    Or(SupportsOr<'s>),
    SupportsInParens(SupportsInParens<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsDecl<'s> {
    pub decl: Declaration<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SupportsInParens<'s> {
    SupportsCondition(SupportsCondition<'s>),
    Feature(SupportsDecl<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsNot<'s> {
    pub ident: Ident<'s>,
    pub condition: SupportsInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsOr<'s> {
    pub ident: Ident<'s>,
    pub condition: SupportsInParens<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct TagNameSelector<'s> {
    pub name: WqName<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum TypeSelector<'s> {
    TagName(TagNameSelector<'s>),
    Universal(UniversalSelector<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct UniversalSelector<'s> {
    pub prefix: Option<NsPrefix<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct UnknownDimension<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct UnquotedFontFamilyName<'s> {
    pub idents: Vec<InterpolableIdent<'s>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Url<'s> {
    pub ident: Ident<'s>,
    pub value: UrlValue<'s>,
    pub span: Span,
}

/// `)` is excluded
#[derive(Clone, Debug, Spanned)]
pub struct UrlRaw<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum UrlValue<'s> {
    Raw(UrlRaw<'s>),
    SassInterpolated(SassInterpolatedUrl<'s>),
    Str(InterpolableStr<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct WqName<'s> {
    pub name: InterpolableIdent<'s>,
    pub prefix: Option<NsPrefix<'s>>,
    pub span: Span,
}
