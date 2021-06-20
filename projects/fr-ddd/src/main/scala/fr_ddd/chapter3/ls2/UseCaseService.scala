package fr_ddd.chapter3.ls2

import fr_ddd.chapter3.ls2.interpreter.common.Amount
import fr_ddd.chapter3.ls2.interpreter.{Account, Balance}

object UseCaseService {

  def openAccount(accountService: AccountService[Account, Amount, Balance]): Unit = {
    import accountService._
//    accountService.open()
    open()
  }
}
