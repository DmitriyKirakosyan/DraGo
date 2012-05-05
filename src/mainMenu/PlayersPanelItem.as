/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 1:04 AM
 */
package mainMenu {
import flash.display.Sprite;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;

import mx.controls.Text;

public class PlayersPanelItem extends Sprite {
	private var _playerName:String;
	private var _playerKey:String;
	private var _textField:TextField;

	public function PlayersPanelItem(playerName:String, playerKey:String) {
		super();
		_playerKey = playerKey;
		_playerName = playerName;
		init();
	}

	public function get playerKey():String { return _playerKey; }
	public function get playerName():String { return _playerName; }


	private function init():void {
		_textField = new TextField();
		_textField.selectable = false;
		_textField.autoSize = TextFieldAutoSize.LEFT;
		_textField.text = _playerName;
		_textField.x = 10;
		_textField.y = 5;
		addChild(_textField);
		graphics.beginFill(0xBEBEBE);
		graphics.drawRect(0, 0, _textField.textWidth + 20, _textField.textHeight + 10);
	}
}
}
