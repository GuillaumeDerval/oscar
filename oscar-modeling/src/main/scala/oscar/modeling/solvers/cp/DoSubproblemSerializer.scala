package oscar.modeling.solvers.cp

import akka.actor.ExtendedActorSystem
import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.twitter.chill.akka.{ActorRefSerializer, AkkaSerializer}
import com.twitter.chill.{IKryoRegistrar, KryoInstantiator, ScalaKryoInstantiator, toRich}
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.{BoolVar, IntVar}

import scala.collection.mutable

/**
  * Custom serializer that avoid sending ModelDeclaration in DoSubproblemMessages (as the MD has already been sent to the Actor)
  */
class DoSubproblemSerializer(system: ExtendedActorSystem) extends AkkaSerializer(system) {
  override def kryoInstantiator: KryoInstantiator =
    (new ScalaKryoInstantiator)
      .withRegistrar(new ActorRefSerializer(system))
      .withRegistrar(new IntVarSerializer(system))
      .withRegistrar(new BoolVarSerializer(system))

  class IntVarSerializer(system: ExtendedActorSystem) extends Serializer[IntVar] with IKryoRegistrar {
    override def write(kryo: Kryo, output: Output, obj: IntVar): Unit = {
      output.writeInt(obj.varid)
      output.writeBoolean(obj.name.isDefined)
      if(obj.name.isDefined)
        output.writeString(obj.name.get)
      kryo.writeClassAndObject(output, obj.model_decl.uuid)
    }

    override def read(kryo: Kryo, input: Input, t: Class[IntVar]): IntVar = {
      val id = input.readInt()
      val name = if(input.readBoolean()) Some(input.readString()) else None
      val uuid = kryo.readClassAndObject(input).asInstanceOf[java.util.UUID]
      new IntVar(DoSubproblemSerializer.get(uuid), id, name)
    }

    override def apply(k: Kryo): Unit = {
      if (!k.alreadyRegistered(classOf[IntVar])) {
        k.forClass[IntVar](this)
        k.forSubclass[IntVar](this)
      }
    }
  }

  class BoolVarSerializer(system: ExtendedActorSystem) extends Serializer[BoolVar] with IKryoRegistrar {
    override def write(kryo: Kryo, output: Output, obj: BoolVar): Unit = {
      output.writeInt(obj.varid)
      if(obj.name.isDefined)
        output.writeString(obj.name.get)
      kryo.writeClassAndObject(output, obj.model_decl.uuid)
    }

    override def read(kryo: Kryo, input: Input, t: Class[BoolVar]): BoolVar = {
      val id = input.readInt()
      val name = if(input.readBoolean()) Some(input.readString()) else None
      val uuid = kryo.readClassAndObject(input).asInstanceOf[java.util.UUID]
      new BoolVar(DoSubproblemSerializer.get(uuid), id, name)
    }

    override def apply(k: Kryo): Unit = {
      if (!k.alreadyRegistered(classOf[BoolVar])) {
        k.forClass[BoolVar](this)
        k.forSubclass[BoolVar](this)
      }
    }
  }
}

object DoSubproblemSerializer {
  private val clientModels = mutable.HashMap[java.util.UUID, ModelDeclaration]()

  /**
    * Add a model declaration for an actor to the registered list. Allow IntVar/BoolVar to be correctly unserialized
    * @param modelDeclaration
    */
  def add(modelDeclaration: ModelDeclaration): Unit = clientModels += ((modelDeclaration.uuid, modelDeclaration))

  /**
    * Allow to spare memory by allowing the GC to remove the ModelDeclaration now unused
    * @param m ModelDeclaration to remove from the list
    */
  def remove(m: ModelDeclaration): Unit = {
    for((uuid, md) <- clientModels)
      if(md == m) {
        clientModels.remove(uuid)
        return
      }
  }

  private def get(m: java.util.UUID): ModelDeclaration = clientModels.getOrElse(m, throw new Exception("Model not registered!"))
}



