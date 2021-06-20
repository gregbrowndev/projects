/* Listing 1.5: Immutable Account model
Now we have improved the previous implement by making the balance within Account
immutable. Now whenever we want to modify the balance, we get back a new Account.

However, Account still holds both state and behaviour. Next, we will decouple the
two to give better modularity and hence better compositionality.
 */
import java.util.{Calendar, Date}

def today: Date = Calendar.getInstance.getTime
type Amount = BigDecimal

case class Balance(amount: Amount = 0)

class Account (
  val no           : String,
  val name         : String,
  dateOfOpening: Date,
  val balance: Balance = Balance()
) {

  def debit(a: Amount): Account = {
    if (balance.amount < a)
      throw new Exception("Insufficient balance in account")
    new Account(no, name, dateOfOpening, Balance(balance.amount - a))
  }

  def credit(a: Amount): Account = new Account(no, name, dateOfOpening, Balance(balance.amount + a))
}

val a = new Account("a1", "John", today)
a.balance == Balance(0)

val b = a.credit(100)
a.balance == Balance(0)
b.balance == Balance(100)

val c = b.debit(20)
b.balance == Balance(100)
c.balance == Balance(80)
