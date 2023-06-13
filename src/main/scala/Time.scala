package reactor
import scala.util.matching.Regex

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

}


object Time {
  def nsec: Long => Time = (n: Long) => new Time(n)
  def usec: Long => Time = (n: Long) => new Time(n * 1000)
  def msec: Long => Time = (n: Long) => new Time(n * 1000000)
  def sec: Long => Time = (n: Long) => new Time(n * 1000000000)
  def nsecs: Long => Time = nsec
  def usecs: Long => Time = usec
  def msecs: Long => Time = msec
  def secs: Long => Time = sec
}


