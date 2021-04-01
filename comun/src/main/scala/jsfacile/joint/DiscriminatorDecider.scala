package jsfacile.joint

/**Configures the subtype-discriminator field of automatically derived translators for the `T` type.
 *
 * Note that the [[jsfacile.annotations.discriminatorField]] annotation has precedence over this decision, so this type-class has no effect on translators for abstract types that are annotated with said annotation.
 *
 * @tparam T the type for which the automatically derived translators would be affected by this [[DiscriminatorDecider]] instance.
 * @tparam F determines which kind of translators are affected by this instance: translators for concrete types ([[jsfacile.joint.ProductsOnly]], for abstract types [[jsfacile.joint.CoproductsOnly]], or both [[jsfacile.joint.AnyAdt]]. */
trait DiscriminatorDecider[T, -F <: AnyAdt] {
	/** Determines the name of the type-discriminator field when one is included */
	def fieldName: String;

	/** Determines when the discriminator field is appended by automatically derived [[jsfacile.write.Appender]]s for abstract types (coproducts). If `true` the discriminator is always appended. If `false` it is appended only if necessary to avoid ambiguity with a sibling subtype. The ambiguity occurs when two or more sibling subtypes have the sane number of required fields and all of them have the same name.
	 *
	 * Automatically derived [[jsfacile.write.Appender]]s for concrete types (products) ignore this member. For them, the discriminator inclusion is determined solely by the existence of an applicable [[DiscriminatorDecider]] value in the implicit scope
	 *
	 * Automatically derived [[jsfacile.read.Parser]]s also ignore this member. They fail only if both, the type-discriminator field is missing, and the names of the parsed fields don't determine a subtype uniquely (two sibling subtypes have the same field names). */
	def required: Boolean;
}

object DiscriminatorDecider {

	/** Summons an instance of [[DiscriminatorDecider]] for the specified type `T`, and translator kind `F`, from the implicit scope. */
	def apply[T, F <: AnyAdt](implicit dd: DiscriminatorDecider[T,  F] = new DiscriminatorDecider[T, F] {
		override val fieldName: String = "?"
		override val required: Boolean = false
	}): DiscriminatorDecider[T, F] = dd;
}


