/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 5/27/12
 * Time: 9:55 PM
 * To change this template use File | Settings | File Templates.
 */
package game.iface.window {
import core.window.IScreenWindow;
import core.window.WindowBase;

import flash.display.Sprite;
import flash.events.MouseEvent;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;

public class EndGameWindow extends WindowBase implements IScreenWindow {
	private const WIDTH:int = 200;
	private const HEIGHT:int = 100;

	private var _okBtn:Sprite;
	private var _resultLabel:TextField;

	public function EndGameWindow() {
		super();
		init();
		x = Main.WIDTH/2 - width/2;
		y = Main.HEIGHT/2 - height/2;
	}

	public function get okBtn():Sprite { return _okBtn; }
	public function setResultText(text:String) { _resultLabel.text = text; }

	private function init():void {
		graphics.beginFill(0, .6);
		graphics.lineStyle(2, 0);
		graphics.drawRect(0, 0, WIDTH, HEIGHT);
		graphics.endFill();
		createLabel();
		createResultLabel();
		initBtns();
	}

	private function initBtns():void {
		_okBtn = new Sprite();
		_okBtn.graphics.beginFill(0xffffff);
		_okBtn.graphics.drawRect(0, 0, 30, 20);
		_okBtn.x = WIDTH/2 - _okBtn.width/2;
		_okBtn.y = HEIGHT - 30;
		var okBtnLabel:TextField = createTextField("Ok");
		okBtnLabel.x = 5;
		okBtnLabel.y = 2;
		_okBtn.addChild(okBtnLabel);
		addChild(_okBtn);
	}

	private function createLabel():void {
		var label:TextField = createTextField("Thanks for the game", 0xffffff);
		label.x = WIDTH/2 - label.textWidth/2;
		label.y = 10;
		addChild(label);
	}

	private function createResultLabel():void {
		_resultLabel = createTextField("THIS IS LOSE!!", 0xffffff);
		_resultLabel.x = WIDTH/2 - _resultLabel.textWidth/2;
		_resultLabel.y = 50;
		addChild(_resultLabel);
	}

	private function createTextField(text:String, color:uint = 0):TextField {
		var result:TextField = new TextField();
		result.selectable = false;
		result.autoSize = TextFieldAutoSize.LEFT;
		result.textColor = color;
		result.text = text;
		result.mouseEnabled = false;
		return result;
	}

}
}
