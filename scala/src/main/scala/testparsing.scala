import spray.json._

case class prices(minSalePrice:Int,minListPrice:Int)

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val parserFormat = jsonFormat2(prices.apply)
}
import MyJsonProtocol._
object testparsing {
  def main(args: Array[String]) = {
    val json = "{ \"Amazon.com Marketplace\" : { \"pid\" : { \"$numberLong\" : \"-6108836322861692713\" }, \"pidStr\" : \"4d6d9c25840d967b79330bec2ffb14ad\", \"url\" : \"https://www.amazon.com/dp/B07HYF693X?psc=1\", \"imageUrl\" : \"https://images-na.ssl-images-amazon.com/images/I/31ZH9WPAVnL.jpg\", \"additionalImageUrls\" : [\"https://images-na.ssl-images-amazon.com/images/I/31ZH9WPAVnL._US800_.jpg\"], \"minSalePrice\" : 22995, \"maxSalePrice\" : 22995, \"minListPrice\" : 22995, \"maxListPrice\" : 22995, \"availability\" : { \"value\" : 0 }, \"lastRecordedAt\" : { \"$numberLong\" : \"1551932684260\" }, \"upcs\" : [\"00798131831555\"], \"mpns\" : [\"T9Y337200\"], \"sku\" : \"B07HYF693X\", \"stdUpcs\" : [], \"asin\" : null, \"isbn\" : null, \"seller\" : \"HCO Success\", \"shippingText\" : \"+ Free Shipping\", \"tags\" : [], \"prevMinSalePrice\" : 22995, \"prevMaxSalePrice\" : 0, \"prevLastRecordedAt\" : { \"$numberLong\" : \"1551932684260\" }, \"fulfilledBy\" : \"HCO Success\", \"newOffers\" : 0, \"usedOffers\" : 0, \"refurbishedOffers\" : 0, \"userRatings\" : 0, \"maxRating\" : 0, \"avgRating\" : 0.0, \"salesRank\" : 0, \"buyBoxWinner\" : true, \"addOnItem\" : false, \"cartPrice\" : false, \"numUnits\" : 0, \"score\" : 0.0, \"discoveredDate\" : { \"$numberLong\" : \"1543079523524\" }, \"discontinuedDate\" : 0, \"webId\" : null, \"addedLastWeek\" : false, \"hasPrivateAttributes\" : false, \"currencyType\" : \"USD\", \"priceChange\" : { \"value\" : 3 }, \"offerId\" : \"4d6d9c25840d967b79330bec2ffb14ad\", \"onPromotion\" : false, \"priceHistoryAvailable\" : false, \"breadCrumbs\" : \"Industrial & Scientific > Occupational Health & Safety Products > Safety Signs & Signals > Safety Barriers > Cones\", \"brandText\" : \"Lakeside Plastics\", \"description\" : \"\", \"processedTime\" : { \"$numberLong\" : \"1551932702128\" }, \"hashedOfferId\" : { \"$numberLong\" : \"-8867821353282820719\" }, \"storeVariantId\" : \"51f77a6bc1126b7df12d8b5954b7b1a8\", \"sourceType\" : { \"value\" : 0 }, \"isPrimePantryItem\" : false, \"gtins\" : [], \"eans\" : [\"0798131831555\"], \"itemCondition\" : { \"value\" : 0 }, \"storesCount\" : 0, \"worstRating\" : 0.0, \"variantId\" : null, \"imageUrlsCount\" : 0, \"psgid\" : \"\", \"paymentMethod\" : [], \"isPrime\" : false } }"
    println(jsonconvert(json))
  }

  def jsonconvert(json: String) = {
    val parsedOffers = json.parseJson.convertTo[Map[String, prices]]
    parsedOffers
  }

}
