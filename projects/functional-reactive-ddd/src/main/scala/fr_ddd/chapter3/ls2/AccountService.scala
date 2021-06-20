/*
Listing 3.2: The algebra in ex3.1 defines the contract for the API of the Account
domain model. In this listing, we will now actually implement the contract. The idea
is to decouple the implementation from the algebra itself so that we can have multiple
implementations for a single algebra. Each implementation is known as the interpreter
of the algebra and will consist of concrete classes and functions that implement the
API definitions.

The trait below declares the contract of AccountService. The concrete implementation can
be found in interpreter/AccountService.
 */

package fr_ddd.chapter3.ls2

import java.util.Date
import scala.util.Try


trait AccountService[Account, Amount, Balance]{
  def open(no: String, name: String, openingDate: Option[Date]): Try[Account]
  def close(account: Account, closeDate: Option[Date]): Try[Account]
  def debit(account: Account, amount: Amount): Try[Account]
  def credit(account: Account, amount: Amount): Try[Account]
  def balance(account: Account): Try[Balance]

  def transfer(from: Account, to: Account, amount: Amount): Try[(Account, Account, Amount)] = for {
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield (a, b, amount)
}