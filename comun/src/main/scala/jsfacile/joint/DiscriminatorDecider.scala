package jsfacile.joint

/** Determines the name of the discriminator field, and if the [[jsfacile.write.CoproductAppender]] must append it ever or only when it's necessary; for all types that are assignable to the specified type `C`.
 * The [[jsfacile.annotations.discriminatorField]] annotation has precedence over this decision, so this type-class has effect only on types that are not annotated with said annotation..
 * */
trait DiscriminatorDecider[-C] {
	/** Determines the name of the discriminator field. */
	def fieldName: String;
	/** Determines if the [[jsfacile.write.CoproductAppender]] must append it ever or only when it's necessary */
	def required: Boolean;
}

object DiscriminatorDecider {

	/** Summons an instance of [[DiscriminatorDecider]] for the specified type from the implicit scope. */
	def apply[C](implicit dd: DiscriminatorDecider[C]): DiscriminatorDecider[C] = dd;
}


