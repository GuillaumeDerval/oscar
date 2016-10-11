package vars

/**
 * A trait from which inherit all implementations of the variables, such as CPVar (for instantiated CP models),
 * DomainStorage (for uninstantiated models), ...
 */
trait VarImplem extends Serializable {}