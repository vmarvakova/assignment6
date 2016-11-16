// Part 2 about Buy-Low-Sell-High using Yahoo Financial Data
//===========================================================


// (1) Complete the function that is given a list of floats
// and calculuates the indices for when to buy the commodity 
// and when to sell


def trade_times(xs: List[Double]): (Int, Int) = {
  if (xs.isEmpty) throw new java.util.NoSuchElementException();

  val minI = xs.indexOf(xs.min)
  val list =xs.drop(minI)
  val mx= list.max

  return  (minI, xs.indexOf(mx))
}

val prices = List(28.0,18.0,20.0,26.0,24.0)
assert(trade_times(prices)==(1,3))



val indices = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")
for (name <- indices) {
  val times = query_company(name)
  println(s"Buy ${name} on ${times._1} and sell on ${times._2}")
}
// an example
//val prices = List(28.0, 18.0, 20.0, 26.0, 24.0)
//assert(trade_times(prices) == (1, 3), "the trade_times test fails")


// (2) Complete the ``get webpage'' function that takes a
// a stock symbol as argument and queries the Yahoo server
// at
//      http://ichart.yahoo.com/table.csv?s=<<insert stock symbol>>
// 
// This servive returns a CSV-list that needs to be separated into
// a list of strings.
import io.Source

def get_page(symbol: String): List[String] = {
  val url = "http://ichart.yahoo.com/table.csv?s=" + symbol
  val line =Source.fromURL(url).getLines.toList
  return line
}

get_page("GOOG")


// (3) Complete the function that processes the CSV list
// extracting the dates and anjusted close prices. The
// prices need to be transformed into Doubles.

def process_page(symbol: String): List[(String,Double)] = {
  val list =get_page(symbol).drop(1)

  var novList = List[(String, Double)]()
  for(line<-list) {
    val smth = line.split(",")
    novList = (smth(0),smth(smth.length-1).toDouble)::novList
  }
  return (novList)

}

process_page("GOOG")
process_page("AAPL")


val companies =
  List("GOOG", "AAPL")

for (s <- companies.par) println(process_page(s))

// (4) Complete the query_company function that obtains the
// processed CSV-list for a stock symbol. It should return
// the dates for when to buy and sell the stocks of that company.

def query_company(symbol: String): (String, String) ={
  var nList = List[Double]()
  val nov= process_page(symbol)
  for(n<-nov.reverse){
    nList = (n._2)::nList
  }
  val l = trade_times(nList)
  val buyTime = nov(l._1)._1
  val sellTime = nov(l._2)._1

  return (buyTime,sellTime)

//  val indices = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")
//  for (name <- indices) {
//    val times = query_company(name)
//    println(s"Buy ${name} on ${times._1} and sell on ${times._2}")
//  }

}


// some test cases

//query_comp("GOOG")

// some more test cases





