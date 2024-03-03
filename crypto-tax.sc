import java.math.MathContext
import java.util.{Calendar, Date}

// TODO: What can I sell?
// TODO: Filter by currency (args)
// TODO: When can I sell the rest?

val TAX_CUTOFF_DATE = {
  val cal = Calendar.getInstance()
  cal.add(Calendar.YEAR, -1)
  cal.getTime
}

case class Args(
               path: String,
               currencyFilters: List[String],
               )
object Args:
  def apply(path: String): Args = Args(path, Nil)
  def apply(path: String, currency: String): Args = Args(path, List(currency))

enum TxType:
  case Deposit, Withdrawal, Buy, Sell, Payouts
object TxType {
  def apply(str: String): TxType = str match {
    case "Deposit" => Deposit
    case "Withdrawal" => Withdrawal
    case "Buy" => Buy
    case "Sell" => Sell
    case "Payouts" => Payouts
    case other => throwException(s"Could not parse transaction type. Received: $other")
  }
}

case class Result(
                 canSell: Amounts,
                 balance: Amounts,
                 )
object Result:
  def empty: Result = Result(Amounts.empty, Amounts.empty)

case class Amounts(netAmount: BigDecimal) {
  def add(netAmount: BigDecimal): Amounts =
    Amounts(this.netAmount + netAmount)

  def sub(netAmount: BigDecimal): Amounts =
    Amounts(this.netAmount - netAmount)
}
object Amounts:
  val empty: Amounts = Amounts(BigDecimal(0))

case class Transaction(
                        localTime: Date,
                        txType: TxType,
                        currency: String,
                        netAmount: BigDecimal,
                        netAmountEur: BigDecimal,
                      )
object Transaction {
  implicit val ordering: Ordering[Transaction] =
    (x: Transaction, y: Transaction) => if (x.localTime.before(x.localTime)) 1 else -1

  val format = new java.text.SimpleDateFormat("yyyy-MM-dd hh:mm:ss")

  def apply(csvLine: String): Transaction =
    csvLine.split(",").map(_.trim).toList match {
      case localTime :: _ :: txType :: currency :: _ :: _ :: _ :: _ :: netAmount :: netAmountEur :: _ =>
        new Transaction(
          format.parse(localTime),
          TxType(txType),
          currency,
          BigDecimal(netAmount, MathContext.DECIMAL128),
          BigDecimal(netAmountEur, MathContext.DECIMAL128)
        )

      case other => throwException(s"Could not build Transaction, received $other")
    }
}

def parseCsv(path: String): Seq[Transaction] = {
    val bufferedSource = io.Source.fromFile(path)
    val txs = bufferedSource
      .getLines()
      .toSeq
      .drop(1)
      .map(Transaction.apply)

    bufferedSource.close

    txs
}

def throwException(msg: String) = throw new Exception(msg)

def moneyIn(acc: Result, date: Date, netAmount: BigDecimal): Result = {
  val newBalance = acc.balance.add(netAmount)
  val newCanSell =
    if (date.before(TAX_CUTOFF_DATE)) acc.canSell.add(netAmount)
    else acc.canSell
  Result(newCanSell, newBalance)
}

def moneyOut(acc: Result, date: Date, netAmount: BigDecimal): Result = {
  val newBalance = acc.balance.sub(netAmount)
  val newCanSell = acc.canSell.sub(netAmount)
  Result(newCanSell, newBalance)
}

def computeCanSellToday(txs: Seq[Transaction]): Map[String, Result] =
  txs
    .groupBy(_.currency)
    .view.mapValues(_.sorted.foldLeft(Result.empty) {
          case (acc, Transaction(date, TxType.Buy, currency, netAmount, _))        =>
            moneyIn(acc, date, netAmount)

          case (acc, Transaction(date, TxType.Deposit, currency, netAmount, _))    =>
            moneyIn(acc, date, netAmount)

          case (acc, Transaction(date, TxType.Payouts, currency, netAmount, _))    =>
            moneyIn(acc, date, netAmount)

          case (acc, Transaction(date, TxType.Sell, currency, netAmount, _))       =>
            moneyOut(acc, date, netAmount)

          case (acc, Transaction(date, TxType.Withdrawal, currency, netAmount, _)) =>
            moneyOut(acc, date, netAmount)

        }
    ).toMap

def parseArgs(args: List[String]): Args =
  args match
    case Nil                     => throwException("Usage: crypto-tax.sc -- /Users/julienderay/Desktop/crypto-tax/sb_statement.csv [BTC]")
    case path :: Nil             => Args(path)
    case path :: currencies      => Args(path, currencies) //TODO: list to filter multiple currencies

def main(rawArgs: List[String]): Unit = {
  val args = parseArgs(rawArgs)
  val txs = parseCsv(args.path).filter(tx => args.currencyFilters.contains(tx.currency))
  val results = computeCanSellToday(txs)

  results.foreach { case (currency, result: Result) =>
    println(s"=== $currency ===")
    println(s"""Balance:  $currency ${result.balance.netAmount}""".stripMargin)
    println(s"""Can sell: $currency ${result.canSell.netAmount}""".stripMargin)
  }
}

main(args.toList)