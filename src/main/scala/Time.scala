package reactor

class Time(val nanoseconds: Long) {
    override def toString: String = s"$ticks ticks"
    def nsec: Long = nanoseconds
    def usec: Long = nanoseconds/1000
    def msec: Long = nanoseconds/1000000
    def sec: Long = nanoseconds/1000000000

    def ticks: Long = nanoseconds
    def toNsecString: String = s"${nanoseconds} nsec"
    def toUsecString: String = s"${usec} usec"
    def toMsecString: String = s"${msec} msec"
    def toSecString: String = s"${sec} sec"

    // Equality check
    def ==(other: Time): Boolean = this.nanoseconds == other.nanoseconds
    def ==(other: Int): Boolean = this.nanoseconds == other

    // Less than and greater than
    def <(other: Time): Boolean = this.nanoseconds < other.nanoseconds
  def <(other: Int): Boolean = this.nanoseconds < other
  def >(other: Time): Boolean = this.nanoseconds > other.nanoseconds
  def >(other: Int): Boolean = this.nanoseconds > other

    // Addition and subtraction
    def +(other: Time): Time = new Time(this.nanoseconds + other.nanoseconds)
    def -(other: Time): Time = new Time(this.nanoseconds - other.nanoseconds)
    def %(other: Time): Time = new Time(this.nanoseconds % other.nanoseconds)

  override def equals(obj: Any): Boolean = obj match {
    case other: Time => this == other
    case _ => false
  }
  override def hashCode: Int = nsec.hashCode
}


object Time {
  implicit val timeOrdering: Ordering[Time] = Ordering.by(_.ticks)
  val FOREVER = new Time(Int.MaxValue)
  val NEVER = new Time(Int.MinValue)
  def nsec: Long => Time = (n: Long) => new Time(n)
}


