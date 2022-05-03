package reactor

import chisel3._
import chisel3.util._

import scala.collection.mutable.ListBuffer

abstract class ReactorElementConfig {
  val id: String
}

abstract class PortConfig extends ReactorElementConfig

abstract class InputPortConfig extends PortConfig {
}
abstract class OutputPortConfig extends PortConfig {
}

case class ReactorInputPortConfig[T <: Data](
 override val id: String,
  numDependencies: Int,
  gen: T
) extends InputPortConfig

case class ReactorOutputPortConfig[T <: Data](
  override val id: String,
  numAntiDependencies: Int,
  gen: T
) extends OutputPortConfig

case class ReactionTriggerConfig[T <: Data](
  override val id: String,
  gen: T
) extends InputPortConfig

case class ReactionAntiDependencyConfig[T <: Data](
  override val id: String,
  gen: T
) extends OutputPortConfig

case class ReactionSchedulePortConfig[T <: Data](
  override val id: String,
  gen: T
) extends OutputPortConfig

case class ReactionDependencyConfig[T <: Data](
  override val id: String,
  gen: T
) extends InputPortConfig

case class SchedulePortConfig[T <: Data](
  override val id: String,
  gen: T
) extends InputPortConfig

case class ActionPortConfig[T <: Data](
  override val id: String,
  gen: T
) extends OutputPortConfig

case class TimerPortConfig(
  override val id: String,
) extends OutputPortConfig

case class ActionConfig[T <: Data](
  override val id: String,
  gen: T,
  out: ActionPortConfig[T],
  in: SchedulePortConfig[T],
  numAntiDependencies: Int,
) extends ReactorElementConfig {
  val eventQueueConfig: EventQueueConfig[T] = EventQueueConfig(8, gen)
}

case class TimerConfig(
  override val id: String,
  interval: Int,
  offset: Int,
  out: TimerPortConfig,
  numAntiDependencies: Int
) extends ReactorElementConfig

case class ReactionConfig(
  override val id: String,
  triggers: Seq[ReactionTriggerConfig[_<:Data]] = Seq(),
  dependencies: Seq[ReactionDependencyConfig[_<:Data]] = Seq(),
  antiDependencies: Seq[ReactionAntiDependencyConfig[_<:Data]] = Seq(),
  schedules: Seq[ReactionSchedulePortConfig[_<:Data]] = Seq()
) extends ReactorElementConfig


abstract class ConnectionType()
case class ReactorReactorConnection() extends ConnectionType
case class ReactorReactionConnection() extends ConnectionType
case class ReactionReactorConnection() extends ConnectionType
case class ActionReactionConnection() extends ConnectionType
case class ReactionActionConnection() extends ConnectionType
case class TimerReactionConnection() extends ConnectionType


case class ConnectionConfig(
  connectionType: ConnectionType,
  srcComponent: ReactorElementConfig,
  dstComponent: ReactorElementConfig,
  srcPort: OutputPortConfig,
  dstPort: InputPortConfig
)

case class ReactorConfig(
  override val id: String,
  top: Boolean,
  actions: Seq[ActionConfig[_<:Data]],
  timers: Seq[TimerConfig],
  reactions: Seq[ReactionConfig],
  inPorts: Seq[ReactorInputPortConfig[_<:Data]],
  outPorts: Seq[ReactorOutputPortConfig[_<:Data]],
  connections: Seq[ConnectionConfig],
  reactors: Seq[ReactorConfig],
  level: Int // Highest number of edges to an input port
) extends ReactorElementConfig {
  def numReactions = reactions.length
  def getReactorIndicesOnLevel(level: Int): Seq[Int] = {
    val res = ListBuffer[Int]()
    for (i <- 0 until reactors.length) {
      if (reactors(i).level == level) {
        res.append(i)
      }
    }
    res.toSeq
  }
  def getNumReactorLevels(): Int = {
    var res = 0
    for (cReactor <- reactors) {
      if (cReactor.level > res) {
        res = cReactor.level
      }
    }
    res
  }

}

case class ReactorGlobalParams(
  numClkBits: Int = 32,
)

object ExampleConfig {

  val action1_out = ActionPortConfig(id="a1_out", gen=UInt(8.W))
  val action1_in = SchedulePortConfig(id="a1_schedule", gen=UInt(8.W))
  val action1 = ActionConfig(id="a1", gen=UInt(8.W), out = action1_out, in = action1_in, numAntiDependencies = 1)

  val timer1_out = TimerPortConfig(id="timer1")
  val timer1 = TimerConfig(id="timer1", interval=1000, offset=500, out = timer1_out, numAntiDependencies = 1)

  val reaction1_in1 = ReactionTriggerConfig(id = "in1", gen = new EmptySignal)
  val reaction1_out = ReactionSchedulePortConfig(id = "out", gen = SInt(8.W))
  val reaction1 = ReactionConfig(id = "reaction1", triggers = Seq(reaction1_in1), schedules = Seq(reaction1_out))

  val reaction2_in1 = ReactionTriggerConfig(id = "in1", gen = UInt(8.W))
  val reaction2 = ReactionConfig(id = "reaction2", triggers = Seq(reaction2_in1))

  val action1_reaction2_conn = ConnectionConfig(
    connectionType = ActionReactionConnection(),
    srcComponent = action1,
    dstComponent = reaction2,
    srcPort = action1_out,
    dstPort = reaction2_in1
  )

  val timer1_reaction1_conn = ConnectionConfig(
    connectionType = ActionReactionConnection(),
    srcComponent = timer1,
    dstComponent = reaction1,
    srcPort = timer1_out,
    dstPort = reaction1_in1
  )

  val reaction1_action1_conn = ConnectionConfig(
    connectionType = ActionReactionConnection(),
    srcComponent = reaction1,
    dstComponent = action1,
    srcPort = reaction1_out,
    dstPort = action1_in
  )

  val topReactor = ReactorConfig(
    id = "main",
    top = true,
    actions = Seq(action1),
    timers = Seq(timer1),
    reactions= Seq(reaction1, reaction2),
    connections = Seq(action1_reaction2_conn, timer1_reaction1_conn, reaction1_action1_conn),
    inPorts = Seq(),
    outPorts = Seq(),
    reactors = Seq(),
    level = -1, // Top Reactor
  )
}

