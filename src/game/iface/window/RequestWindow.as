/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 5:32 PM
 */
package game.iface.window {
import core.window.IScreenWindow;
import core.window.WindowBase;

import flash.display.Sprite;
import flash.events.MouseEvent;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;

import mx.controls.Text;

public class RequestWindow extends WindowBase implements IScreenWindow {
	private const WIDTH:int = 200;
	private const HEIGHT:int = 100;

	private var _okBtn:Sprite;
	private var _exitBtn:Sprite;

	public function RequestWindow() {
		super();
		init();
		x = Main.WIDTH/2 - width/2;
		y = Main.HEIGHT/2 - height/2;
	}

	private function init():void {
		graphics.beginFill(0, .6);
		graphics.lineStyle(2, 0);
		graphics.drawRect(0, 0, WIDTH, HEIGHT);
		graphics.endFill();
		createLabel();
		initBtns();
	}

	private function createLabel():void {
		var label:TextField = createTextField("Play request", 0xffffff);
		label.x = WIDTH/2 - label.textWidth/2;
		label.y = 10;
		addChild(label);
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
		_okBtn.addEventListener(MouseEvent.CLICK, onOkBtnClick);
		addChild(_okBtn);
		_exitBtn = new Sprite();
		_exitBtn.graphics.beginFill(0xffffff);
		_exitBtn.graphics.drawCircle(10, 10, 10);
		var exitBtnLabel:TextField = createTextField("X");
		exitBtnLabel.x = 3;
		exitBtnLabel.y = 3;
		_exitBtn.x = WIDTH - _exitBtn.width/2;
		_exitBtn.y = -_exitBtn.height/2;
		_exitBtn.addChild(exitBtnLabel);
		_exitBtn.addEventListener(MouseEvent.CLICK, onExitBtnClick);
		addChild(_exitBtn);
	}

	private function onOkBtnClick(event:MouseEvent):void {
		trace("ok clicked");
	}
	private function onExitBtnClick(event:MouseEvent):void {
		trace("exit clicked");
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
