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
pub struct AtRule<'a> {
    pub name: Ident<'a>,
    pub prelude: Option<AtRulePrelude<'a>>,
    pub block: Option<SimpleBlock<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum AtRulePrelude<'a> {
    Charset(Str<'a>),
    CustomMedia(CustomMedia<'a>),
    Keyframes(KeyframesName<'a>),
    Media(MediqQueryList<'a>),
    Supports(SupportsCondition<'a>),
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
    pub ident: InterpolableIdent<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum AttributeSelectorValue<'a> {
    Ident(InterpolableIdent<'a>),
    Str(InterpolableStr<'a>),
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
pub struct ClassSelector<'a> {
    pub name: InterpolableIdent<'a>,
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
    InterpolableStr(InterpolableStr<'a>),
    LessVariable(LessVariable<'a>),
    Number(Number<'a>),
    Percentage(Percentage<'a>),
    Ratio(Ratio<'a>),
    SassBinaryExpression(SassBinaryExpression<'a>),
    SassParenthesizedExpression(SassParenthesizedExpression<'a>),
    SassUnaryExpression(SassUnaryExpression<'a>),
    SassVariable(SassVariable<'a>),
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
pub struct CustomMedia<'a> {
    pub name: InterpolableIdent<'a>,
    pub value: CustomMediaValue<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum CustomMediaValue<'a> {
    MediaQueryList(MediqQueryList<'a>),
    True(Ident<'a>),
    False(Ident<'a>),
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
    pub name: InterpolableIdent<'a>,
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
    LessInterpolated(LessInterpolatedIdent<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct InterpolableIdentStaticPart<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum InterpolableStr<'a> {
    Literal(Str<'a>),
    SassInterpolated(SassInterpolatedStr<'a>),
    LessInterpolated(LessInterpolatedStr<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct InterpolableStrStaticPart<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct IdSelector<'a> {
    pub name: InterpolableIdent<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct KeyframeBlock<'a> {
    pub prelude: Vec<KeyframeSelector<'a>>,
    pub block: SimpleBlock<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum KeyframeSelector<'a> {
    Ident(InterpolableIdent<'a>),
    Percentage(Percentage<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub enum KeyframesName<'a> {
    Ident(InterpolableIdent<'a>),
    Str(InterpolableStr<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct LessInterpolatedIdent<'a> {
    pub elements: Vec<LessInterpolatedIdentElement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum LessInterpolatedIdentElement<'a> {
    Variable(LessVariableInterpolation<'a>),
    Static(InterpolableIdentStaticPart<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct LessInterpolatedStr<'a> {
    pub elements: Vec<LessInterpolatedStrElement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum LessInterpolatedStrElement<'a> {
    Variable(LessVariableInterpolation<'a>),
    Static(InterpolableStrStaticPart<'a>),
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
pub struct LessVariable<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessVariableDeclaration<'a> {
    pub name: LessVariable<'a>,
    pub value: ComponentValues<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessVariableInterpolation<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Length<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaAnd<'a> {
    pub ident: Ident<'a>,
    pub media_in_parens: MediaInParens<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaCondition<'a> {
    pub conditions: Vec<MediaConditionKind<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaConditionKind<'a> {
    MediaInParens(MediaInParens<'a>),
    And(MediaAnd<'a>),
    Or(MediaOr<'a>),
    Not(MediaNot<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaFeature<'a> {
    Plain(MediaFeaturePlain<'a>),
    Boolean(MediaFeatureBoolean<'a>),
    Range(MediaFeatureRange<'a>),
    RangeInterval(MediaFeatureRangeInterval<'a>),
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
pub enum MediaFeatureName<'a> {
    Ident(InterpolableIdent<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureBoolean<'a> {
    pub name: MediaFeatureName<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeaturePlain<'a> {
    pub name: MediaFeatureName<'a>,
    pub value: ComponentValue<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureRange<'a> {
    pub left: ComponentValue<'a>,
    pub comparison: MediaFeatureComparison,
    pub right: ComponentValue<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaFeatureRangeInterval<'a> {
    pub left: ComponentValue<'a>,
    pub left_comparison: MediaFeatureComparison,
    pub name: MediaFeatureName<'a>,
    pub right_comparison: MediaFeatureComparison,
    pub right: ComponentValue<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaInParens<'a> {
    MediaCondition(MediaCondition<'a>),
    MediaFeature(MediaFeature<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaNot<'a> {
    pub ident: Ident<'a>,
    pub media_in_parens: MediaInParens<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaOr<'a> {
    pub ident: Ident<'a>,
    pub media_in_parens: MediaInParens<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum MediaQuery<'a> {
    ConditionOnly(MediaCondition<'a>),
    WithType(MediaQueryWithType<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct MediqQueryList<'a> {
    pub queries: Vec<MediaQuery<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct MediaQueryWithType<'a> {
    pub modifier: Option<Ident<'a>>,
    pub media_type: InterpolableIdent<'a>,
    pub condition: Option<MediaCondition<'a>>,
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
    Ident(InterpolableIdent<'a>),
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
pub struct Ratio<'a> {
    pub numerator: Number<'a>,
    pub denominator: Number<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Resolution<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassBinaryExpression<'a> {
    pub left: Box<ComponentValue<'a>>,
    pub op: BinaryOperator,
    pub right: Box<ComponentValue<'a>>,
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
    Static(InterpolableIdentStaticPart<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SassInterpolatedStr<'a> {
    pub elements: Vec<SassInterpolatedStrElement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SassInterpolatedStrElement<'a> {
    Expression(ComponentValues<'a>),
    Static(InterpolableStrStaticPart<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SassParenthesizedExpression<'a> {
    pub expr: Box<ComponentValue<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassPlaceholderSelector<'a> {
    pub name: InterpolableIdent<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SassUnaryExpression<'a> {
    pub op: SassUnaryOperator,
    pub expr: Box<ComponentValue<'a>>,
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
pub struct SassWarnAtRule<'a> {
    pub expr: ComponentValues<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SelectorList<'a> {
    pub selectors: Vec<ComplexSelector<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SimpleBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SimpleSelector<'a> {
    Class(ClassSelector<'a>),
    Id(IdSelector<'a>),
    Type(TypeSelector<'a>),
    Attribute(AttributeSelector<'a>),
    Nesting(NestingSelector),
    SassPlaceholder(SassPlaceholderSelector<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub enum Statement<'a> {
    AtRule(AtRule<'a>),
    Declaration(Declaration<'a>),
    KeyframeBlock(KeyframeBlock<'a>),
    LessVariableDeclaration(LessVariableDeclaration<'a>),
    QualifiedRule(QualifiedRule<'a>),
    SassVariableDeclaration(SassVariableDeclaration<'a>),
    SassWarnAtRule(SassWarnAtRule<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Str<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Stylesheet<'a> {
    pub statements: Vec<Statement<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsAnd<'a> {
    pub ident: Ident<'a>,
    pub condition: SupportsInParens<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsCondition<'a> {
    pub conditions: Vec<SupportsConditionKind<'a>>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SupportsConditionKind<'a> {
    Not(SupportsNot<'a>),
    And(SupportsAnd<'a>),
    Or(SupportsOr<'a>),
    SupportsInParens(SupportsInParens<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsDecl<'a> {
    pub decl: Declaration<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum SupportsInParens<'a> {
    SupportsCondition(SupportsCondition<'a>),
    Feature(SupportsDecl<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsNot<'a> {
    pub ident: Ident<'a>,
    pub condition: SupportsInParens<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct SupportsOr<'a> {
    pub ident: Ident<'a>,
    pub condition: SupportsInParens<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct TagNameSelector<'a> {
    pub name: WqName<'a>,
    pub span: Span,
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
    pub name: InterpolableIdent<'a>,
    pub prefix: Option<NsPrefix<'a>>,
    pub span: Span,
}
