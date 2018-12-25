package exo.auction

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.util.control.NonFatal

object AuctionHouse {
	sealed trait Command
 	case class AuctioneerCommand(command: AuctioneerCommand.Command) extends Command
	object AuctioneerCommand {
		sealed trait Command
		case class Create(client: ActorRef[Create.Response], initial: InitialAuction) extends Command
		object Create {
			case class Response(ok: Boolean)
		}
		case class Update(client: ActorRef[Update.Response], initial: InitialAuction) extends Command
		object Update {
			case class Response(ok: Boolean)
		}
		case class Interrogate(client: ActorRef[Interrogate.Response], item: String) extends Command
		object Interrogate {
			sealed trait Response
			case class Found(history: Seq[Bid]) extends Response
			case object NotFound extends Response
		}
	}
	case class BidderCommand(bidder: Bidder, command: BidderCommand.Command) extends Command
	object BidderCommand {
		sealed trait Command
		case class Interrogate(client: ActorRef[Interrogate.Response], states: Set[Auction.State], mine: Boolean) extends Command
		object Interrogate {
			case class Response(auctions: Seq[Auction])//TODO looks big: need the functional use case to reduce this
		}
		case class Join(client: ActorRef[Join.Response], item: String) extends Command
		object Join {
			trait Response
			case object Joined extends Response
			case class Denied(e: Throwable) extends Response
			case object NotFound extends Response
		}
		case class Bid(client: ActorRef[Bid.Response], item: String, price: Double) extends Command
		object Bid {
			sealed trait Response
			case object Put extends Response
			case class Denied(e: Throwable) extends Response
			case object NotFound extends Response
		}
	}

	case class State(auctions: Map[String, Auction]) {
		def set(state: Auction) = State(auctions + (state.item -> state))
		def add(state: Auction) = {
			require(!auctions.contains(state.item))
			set(state)
		}
		def update(initial: InitialAuction) = set(auctions(initial.item).update(initial))
	}
	object State {
		object Empty extends State(Map.empty)
	}

	val behavior: Behavior[Command] = auctionHouse(State.Empty)

	private def auctionHouse(state: State): Behavior[Command] = Behaviors.receive {(_, message) => message match {
		case AuctioneerCommand(command) => command match {
			case AuctioneerCommand.Create(client, initial) => try {
				val behavior = auctionHouse(state.add(Auction(initial)))
				client ! AuctioneerCommand.Create.Response(true)
				behavior
			} catch {case NonFatal(_) =>
				client ! AuctioneerCommand.Create.Response(false)
				Behaviors.same
			}
			case AuctioneerCommand.Update(client, initial) => try {
				val behavior = auctionHouse(state.update(initial))
				client ! AuctioneerCommand.Update.Response(true)
				behavior
			} catch {case NonFatal(_) =>
				client ! AuctioneerCommand.Update.Response(false)
				Behaviors.same
			}
			case AuctioneerCommand.Interrogate(client, item) =>
				client ! (state.auctions.get(item) match {
					case Some(auction) => AuctioneerCommand.Interrogate.Found(auction.bids)
					case None => AuctioneerCommand.Interrogate.NotFound
				})
				Behaviors.same
		}
		case BidderCommand(bidder, command) => command match {
			case BidderCommand.Interrogate(client, states, mine) =>
				val filter: Auction => Boolean =
					if (mine)
						(auctionState: Auction) => auctionState.bidders(bidder) && states(auctionState.state)
					else
						(auctionState: Auction) => states(auctionState.state)
				client ! BidderCommand.Interrogate.Response(state.auctions.values.filter(filter).toSeq)
				Behaviors.same
			case BidderCommand.Join(client, item) => state.auctions.get(item) match {
				case Some(auction) =>
					val (message, behavior: Behavior[Command]) = try {
						val behavior = auctionHouse(state.set(auction.join(bidder)))
						(BidderCommand.Join.Joined, behavior)
					} catch {case NonFatal(e) =>
						(BidderCommand.Join.Denied(e), Behaviors.same)
					}
					client ! message
					behavior
				case None =>
					client ! BidderCommand.Join.NotFound
					Behaviors.same
			}
			case BidderCommand.Bid(client, item, price) => state.auctions.get(item) match {
				case Some(auction) =>
					val (message, behavior: Behavior[Command]) = try {
						val behavior = auctionHouse(state.set(auction.bid(bidder, price)))
						(BidderCommand.Bid.Put, behavior)
					} catch {case NonFatal(e) =>
						(BidderCommand.Bid.Denied(e), Behaviors.same)
					}
					client ! message
					behavior
				case None =>
					client ! BidderCommand.Bid.NotFound
					Behaviors.same
			}
		}
	}}

	def createSystem = ActorSystem(behavior, "action-house")
}
