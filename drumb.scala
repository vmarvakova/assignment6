import scala.io.Source

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CBG", "CCI",
  "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "GGP", "HCP")


def get_first_price(symbol: String, year: Int): Option[Double] = {
  var novList=""
  val url = "http://ichart.yahoo.com/table.csv?s=" + symbol + "&a=0&b=1&c=" + year + "&d=1&e=1&f=" + year
  val list = Source.fromURL(url).getLines.toList
  for(line<-list) {
    val smth = line.split(",")
    novList = (smth(smth.length-1))
  }
  novList.headOption
  Option(novList.toDouble)
}
get_first_price("GOOG",2010)
get_first_price("GOOG",2012)
get_first_price("AAPL",2010)
get_first_price("AAPL",2011)
def get_prices(portfolio: List[String], years: Range): List[List[Option[Double]]]= {

  var listLists:List[List[Option[Double]]]=List()
  for (b<-years){
    var listPrices:List[Option[Double]] = List()
    for (a<-portfolio){
      listPrices=get_first_price(a,b)::listPrices
    }
    listLists=listPrices.reverse::listLists
  }
  listLists.reverse
}

val p =get_prices(List("GOOG","AAPL"), 2010 to 2012)
assert(get_prices(List("GOOG","AAPL"), 2010 to 2012)==(List(List(Some(313.062468), Some(27.847252)),
  List(Some(301.873641), Some(42.884065)),
  List(Some(332.373186), Some(53.509768)))))

def get_delta(price_old: Option[Double], price_new: Option[Double]): Option[Double] = {

  val formulae =((price_new.get - price_old.get)/price_old.get)
  Option(formulae)
}
get_delta(get_first_price("GOOG",2010),get_first_price("GOOG",2012))


def get_deltas(data: List[List[Option[Double]]]):  List[List[Option[Double]]] = {
  var listDeltas:List[Option[Double]]=List()

  for (end <- data.tail.indices) {
    for (ind <- data.apply(end).indices)
      if (get_delta(data.apply(end).apply(ind),data.apply(end+1).apply(ind))!= Some((0.0)))
        listDeltas=get_delta(data.apply(end).apply(ind),data.apply(end+1).apply(ind))::listDeltas
  }
  val finalList: List[List[Option[Double]]] = List(listDeltas.reverse).reverse
  finalList
}

// test case using the prices calculated above
val d = get_deltas(p)


def yearly_yield(data: List[List[Option[Double]]], balance: Long, year: Int): Long = {
  val invests=data.apply(year).size
  var bal:Double=0.0
  for (newData <- data(year)){
    bal+=(newData.get * (balance/invests))
  }
  val finalV = bal.toLong + balance
  finalV
}

//test case
yearly_yield(d, 100, 0)

def compound_yield(data: List[List[Option[Double]]], balance: Long, year: Int): Long = {
  var finalB=balance
  for (years <- 0 to year) {
    finalB=yearly_yield(data,finalB,years)
  }
  finalB
}


def investment(portfolio: List[String], years: Range, start_balance: Long): Long = {
  val priceList = get_prices(portfolio,years)
  val deltaList = get_deltas(p)
  compound_yield(deltaList, start_balance, years.tail.indices.last)

}
