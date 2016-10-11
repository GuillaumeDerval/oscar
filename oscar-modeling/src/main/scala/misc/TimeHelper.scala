package misc

import java.lang.management.ManagementFactory

import com.sun.management.OperatingSystemMXBean

/**
  * Created by dervalguillaume on 16/11/15.
  */
object TimeHelper {
  def getThreadCpuTime: Long = {
    val bean = ManagementFactory.getThreadMXBean
     if(bean.isCurrentThreadCpuTimeSupported)
       bean.getCurrentThreadCpuTime
     else
       0L
  }

  def getTotalCpuTime: Long = {
    val bean =ManagementFactory.getOperatingSystemMXBean
    bean.asInstanceOf[OperatingSystemMXBean].getProcessCpuTime
  }

  def getClockTime: Long = System.nanoTime()
}
