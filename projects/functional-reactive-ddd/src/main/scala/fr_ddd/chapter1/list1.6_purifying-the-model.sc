/* Listing 1.6: Purifying the model
The previous example still held both state and behaviour in Account.
Now, we have decoupled them by moving the credit and debit methods on
an AccountService.

The state resides in Account which is now an immutable abstract data type (ADT),
i.e. it doesn't contain any data, and the behaviours in the service/module.

Services such as AccountService are defined in modules which in Scala are
implemented as traits. Traits act as mixins and enable easy composition. When
need a concrete instance of a module, you use the object keyword.

Note, we also change the way we deal with Exceptions, using Scala's Try, Success,
Failure monads.
*/

import java.util.{Calendar, Date}
import scala.util.{Failure, Success, Try}

def today: Date = Calendar.getInstance.getTime
type Amount = BigDecimal

case class Balance(amount: Amount = 0)

case class Account(
  no           : String,
  name         : String,
  dateOfOpening: Date,
  balance      : Balance = Balance()
)

trait AccountService {
  def debit(account: Account, amount: Amount): Try[Account] = {
    if (account.balance.amount < amount)
      Failure(new Exception("Insufficient balance in account"))
    else Success(account.copy(balance = Balance(account.balance.amount - amount)))
  }

  def credit(account: Account, amount: Amount): Try[Account] =
    Success(account.copy(balance = Balance(account.balance.amount + amount)))
}

object AccountService extends AccountService
import AccountService._

val a = new Account("a1", "John", today)

// Advanced function composition possible thanks to Try abstraction
for {
  b <- credit(a, 1000)
  c <- debit(b, 200)
  d <- debit(c, 190)
} yield d
