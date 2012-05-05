/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 12:57 AM
 */
package mainMenu {
import flash.display.Sprite;
import flash.events.Event;

import game.staticModel.UserState;

public class PlayersPanel extends Sprite {
	private const WIDTH:int = 200;
	private const HEIGHT:int = 200;

	private var _items:Vector.<PlayersPanelItem>;

	public function PlayersPanel() {
		super();
		init();
		UserState.instance.addEventListener(Event.CHANGE, onUserStateChange);
	}

	private function init():void {
		this.graphics.beginFill(0xD3D3D3);
		this.graphics.lineStyle(1, 0);
		this.graphics.drawRect(0, 0, WIDTH, HEIGHT);
		this.graphics.endFill();
	}

	private function onUserStateChange(event:Event):void {
		if (!_items || playersChanged()) {
			trace("users changed [PlayersPanel.onUserStateChange]");
			removeItems();
			createItems();
		}
	}

	private function removeItems():void {
		if (!_items) { return; }
		for each (var item:PlayersPanelItem in _items) {
			if (super.contains(item)) { super.removeChild(item); }
		}
		_items = new Vector.<PlayersPanelItem>();
	}

	private function createItems():void {
		if (!_items) { _items = new Vector.<PlayersPanelItem>(); }
		var playerItem:PlayersPanelItem;
		var i:int = 0;
		for each (var userKey:String in UserState.instance.users) {
			playerItem = new PlayersPanelItem("player " + i, userKey);
			_items.push(playerItem);
			playerItem.y = 5 + i * (playerItem.height + 10);
			playerItem.x = 10;
			addChild(playerItem);
			++i;
		}
	}

	private function playersChanged():Boolean {
		if (_items.length != UserState.instance.users.length) {
			return true;
		}
		for (var i:int = 0; i < _items.length; ++i) {
			if (_items[i].playerKey != UserState.instance.users[i]) {
				return true;
			}
		}
		return false;
	}
}
}
