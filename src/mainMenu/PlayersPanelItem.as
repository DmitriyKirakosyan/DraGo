/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 1:04 AM
 */
package mainMenu {
import flash.display.Sprite;
import flash.events.MouseEvent;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;

import game.manager.RequestManager;

public class PlayersPanelItem extends Sprite {
	private var _playerName:String;
	private var _playerKey:String;
	private var _textField:TextField;

	public function PlayersPanelItem(playerName:String, playerKey:String) {
		super();
		_playerKey = playerKey;
		_playerName = playerName;
		init();
		addListeners();
	}

	public function remove():void {
		removeListeners();
	}

	public function get playerKey():String { return _playerKey; }
	public function get playerName():String { return _playerName; }


	private function addListeners():void {
		addEventListener(MouseEvent.MOUSE_OVER, onMouseOver);
		addEventListener(MouseEvent.MOUSE_OUT, onMouseOut);
		addEventListener(MouseEvent.CLICK, onClick);
	}
	private function removeListeners():void {
		removeEventListener(MouseEvent.MOUSE_OVER, onMouseOver);
		removeEventListener(MouseEvent.MOUSE_OUT, onMouseOut);
		removeEventListener(MouseEvent.CLICK, onClick);
	}

	private function onMouseOver(event:MouseEvent):void {
		graphics.beginFill(0xB9D3EE);
		graphics.drawRect(0, 0, _textField.textWidth + 20, _textField.textHeight + 10);
	}
	private function onMouseOut(event:MouseEvent):void {
		graphics.beginFill(0xBEBEBE);
		graphics.drawRect(0, 0, _textField.textWidth + 20, _textField.textHeight + 10);
	}

	private function onClick(event:MouseEvent):void {
		RequestManager.instance.makeRequest(_playerKey);
	}

	private function init():void {
		_textField = new TextField();
		_textField.selectable = false;
		_textField.autoSize = TextFieldAutoSize.LEFT;
		_textField.mouseEnabled = false;
		_textField.text = _playerName;
		_textField.x = 10;
		_textField.y = 5;
		addChild(_textField);
		graphics.beginFill(0xBEBEBE);
		graphics.drawRect(0, 0, _textField.textWidth + 20, _textField.textHeight + 10);
	}
}
}
