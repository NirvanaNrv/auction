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
			case class Response(history: Seq[Bid])
		}
	}
	case class BidderCommand(bidder: Bidder, command: BidderCommand.Command) extends Command
	object BidderCommand {
		sealed trait Command
		case class Interrogate(client: ActorRef[Interrogate.Response], states: Set[Auction.State], mine: Boolean) extends Command
		object Interrogate {
			case class Response(auctions: Seq[Auction]) //TODO looks big: need the functional use case to reduce this
		}
		case class Join(client: ActorRef[Join.Response], item: String) extends Command
		object Join {
			case class Response(ok: Boolean)
		}
		case class Bid(client: ActorRef[Bid.Response], item: String, price: Double) extends Command
		object Bid {
			case class Response(ok: Boolean)
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
				client ! AuctioneerCommand.Interrogate.Response(state.auctions(item).bids)
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
			case BidderCommand.Join(client, item) => try {
				val behavior = auctionHouse(state.set(state.auctions(item).join(bidder)))
				client ! BidderCommand.Join.Response(true)
				behavior
			} catch {case NonFatal(_) =>
				client ! BidderCommand.Join.Response(false)
				Behaviors.same
			}
			case BidderCommand.Bid(client, item, price) => try {
				val behavior = auctionHouse(state.set(state.auctions(item).bid(bidder, price)))
				client ! BidderCommand.Bid.Response(true)
				behavior
			} catch {case NonFatal(_) =>
				client ! BidderCommand.Bid.Response(false)
				Behaviors.same
			}
		}
	}}

	val system = ActorSystem(behavior, "hello")
}
