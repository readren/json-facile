package jsfacile.joint

/**Configures the subtype-discriminator field appended by automatically derived [[jsfacile.write.Appender]]s, and expected by automatically derived [[jsfacile.read.Parser]]s, for abstract types (coproducts).
 *
 * The [[jsfacile.annotations.discriminatorField]] annotation has precedence over this decision, so this type-class has effect only on abstract types that are not annotated with said annotation.
 * @tparam A the type for which the automatically derived translators would be affected by this [[DiscriminatorDecider]] instance.
 * @tparam F determines which kind of translators are affected by this instance: translators for concrete types ([[jsfacile.joint.ProductsOnly]], for abstract types [[jsfacile.write.PrefixInserter.CoproductsOnly]], or both [[jsfacile.write.PrefixInserter.AnyAdt]]. */
trait DiscriminatorDecider[C, -F <: AnyAdt] {
	/** Determines the name of the discriminator field. */
	def fieldName: String;
	/** Determines when the discriminator field is appended by automatically derived [[jsfacile.write.Appender]]s. If `true` the discriminator is always appended. If `false` it is appended only if necessary to avoid ambiguity with a sibling subtype. The ambiguity occurs when two or more sibling subtypes have the sane number of required fields and all of them have the same name.
	 *
	 * Automatically derived [[jsfacile.read.Parser]]s ignore this member. They fail if the type-discriminator field is missing and the subtype has ambiguity with a sibling. */
	def required: Boolean;
}

object DiscriminatorDecider {

	/** Summons an instance of [[DiscriminatorDecider]] for the specified type from the implicit scope. */
	def apply[C, F <: AnyAdt](implicit dd: DiscriminatorDecider[C,  F] = new DiscriminatorDecider[C, F] {
		override val fieldName: String = "?"
		override val required: Boolean = false
	}): DiscriminatorDecider[C, F] = dd;
}


