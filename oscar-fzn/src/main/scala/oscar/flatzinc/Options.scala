/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/**
 * @author Leonard Debroux
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */

package oscar.flatzinc

import scala.collection.mutable.Map

class Options(name: String, cbls: Boolean, args: Array[String]) {
  //println("options:"+args.mkString("%"))
  val progName = name

  
  //var file: FileReader = null
  var fileName: String = null
  var all = false
  var verbose = 0
  var statistics = false
  var timeOut = 0
  var nSols = 1
  var help = false;
  var freeSearch = false;
  
  private val opts = Map.empty[String,String]
  def is(s: String): Boolean = {
    //println("% checking option: "+s)
    opts.contains(s) && opts(s)=="true"
  }
  def get(s: String): Option[String] = {
    //println("% checking option: "+s)
    opts.get(s)
  }
  
  def log(): Log = new Log(verbose)
  
  if (args.length == 0) {
    System.out.println(progName+": no model file specified");
    System.out.println(progName+": use --help for more information.");
    System.exit(0);
  } else if (args.length == 1) {
    val arg = args(0)
    if (arg.equals("-h") || arg.equals("--help")) {
      help = true;
    } else { // input file
      fileName = args(0)
    }
  } else { // args.length > 1
    var i = 0
    def oneOf(poss: String*) = poss.exists(args(i).equals(_))
    while (i < args.length - 1) {
      if (oneOf("-h","--help")) {
    	help = true;
      }else if (oneOf("-a", "--all-solutions", "--all")) {
        all = true
      } else if (oneOf("-t", "--time-out")) {
        timeOut = args(i+1).toInt
        i += 1
      } else if (oneOf("-s", "--statistics")) {
        statistics = true
      } else if (oneOf("-n", "--num-solutions")) {
        nSols = args(i+1).toInt
        i += 1
      } else if (oneOf("-v", "--verbose")) {
        verbose = 1;
      } else if (oneOf("-f")) {
        freeSearch = true;
      }else if (oneOf("-vl", "--verbose-level")) {
        verbose = args(i+1).toInt
        i += 1
      } else if (args(i).startsWith("-X")){
        val parts = args(i).substring(2).split("=", 2)
        opts(parts(0)) = if(parts.length==1) "true" else parts(1)
      } else {
        println(progName+": not recognized option " + args(i))
        help = true;
      }
      i+=1
    }
	fileName = args.last
  }
  if(help){
    println(
      "Usage: "+progName+" [<options>] <file>.fzn\n" +
        "Options:\n" +
        "    -h, --help\n" +
        "        Print this message.\n" +
        "    -a, --all, --all-solutions\n" +
        "    -v, --verbose\n" +
        "    -t <value>, --time-out <value>\n" +
        "        <value> - time in second.\n" +
        "    -s, --statistics\n" +
        "    -n <value>, --num-solutions <value>\n" +
        "        <value> - limit on solution number.\n"+
        "    -X<opt>=<val>, -X<opt>\n" +
        "        <opt> and <val> - add advanced options to the back-end\n")
    System.exit(0);
  }
  if(cbls && (all || nSols != 1))
      println("% The CBLS Backend does not support the options to get all solutions or more than one solution.");
}
