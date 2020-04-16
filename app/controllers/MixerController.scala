package controllers

import javax.inject.Inject
import play.api.data._
import play.api.i18n._
import play.api.mvc._
import services.ErgoMixingSystem
import scala.util.Try

/**
  * Instead of MessagesAbstractController, you can use the I18nSupport trait,
  * which provides implicits that create a Messages instance from a request
  * using implicit conversion.
  *
  * See https://www.playframework.com/documentation/2.8.x/ScalaForms#passing-messagesprovider-to-form-helpers
  * for details.
  */
class MixerController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  import MixForm._

  private val postUrl = routes.MixerController.createMix()

  private lazy val ergoMixer = ErgoMixingSystem.ergoMixer

  def index = Action {
    Ok(views.html.index())
  }

  def newMix = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.newMix(form, postUrl))
  }

  def mixingStats = Action {
    val mixRequests = ergoMixer.getMixes.map(_.mixRequest).reverse
    Ok(views.html.mixingStats(mixRequests))
  }

  def ringStats = Action {
    Ok(views.html.ringStats())
  }

  // This will be the action that handles our form post
  def createMix = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[NewMixData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.newMix(formWithErrors, postUrl))
    }

    val successFunction = { data: NewMixData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      println(Try(ergoMixer.newMixRequest(30, data.withdrawalAddress)))
      Redirect(routes.MixerController.mixingStats()).flashing("info" -> "Mix added!")
    }

    println("processing: ")
    val formValidationResult = form.bindFromRequest
    println("validation: " + formValidationResult)
    formValidationResult.fold(errorFunction, successFunction)
  }
}
