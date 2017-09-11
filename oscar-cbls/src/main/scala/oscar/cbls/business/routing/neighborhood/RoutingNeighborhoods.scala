package oscar.cbls.business.routing.neighborhood

/**
 * Created by rdl on 11-09-17.
 */
trait RoutingNeighborhoods
  extends InsertPointAPI
  with OnePointMovsAPI
  with RemovePointAPI
  with RouteExchangeAPI
  with SegmentExchangeAPI
  with ThreeOptAPI
  with TwoOptAPI{

}
