package fpinscala.applicative

import org.scalatest.{FunSpec, Matchers}

import fpinscala.state.State

class ApplicativeSpec extends FunSpec with Matchers {
  describe("Applicative Spec") {
    it("Should properly use validationApplicative") {
      import Validation._
      import java.text._

      val name1 = "Bob"
      val nameEmpty = ""
      val goodBirthdateInput = "1919-02-03"
      val goodBirthdateOutput = (new SimpleDateFormat("yyyy-MM-dd")).parse(goodBirthdateInput)
      val badBirthdateInput = "32456478adsagf"
      val goodPhoneNumber = "1234567890"
      val badPhoneNumber = "123"

      val expected1 = Success(WebForm(name1, goodBirthdateOutput, goodPhoneNumber))
      val expected2 = Failure("Name cannot be empty")
      val expected3 = Failure(
        "Birthdate must be in the form yyyy-MM-dd",
        Vector("Phone number must be 10 digits")
      )

      validWebForm(name1, goodBirthdateInput, goodPhoneNumber) shouldBe
        expected1
      validWebForm(nameEmpty, goodBirthdateInput, goodPhoneNumber) shouldBe
        expected2
      validWebForm(name1, badBirthdateInput, badPhoneNumber) shouldBe
        expected3
    }
  }

}
