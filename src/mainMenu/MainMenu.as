package mainMenu {
import com.greensock.TweenMax;

import controller.ViewController;

import flash.display.Sprite;
import flash.events.MouseEvent;

import scene.IScene;
import scene.SceneEvent;

public class MainMenu extends ViewController implements IScene {
	private var _dashboard:Dashboard;
	private var _container:Sprite;
	private var _startButton:StartButtonView;

	public function MainMenu(container:Sprite) {
		super(new MenuView());
		_container = container;
		initObjects();
	}

	public function open():void {
		_container.addChild(view);
		_container.addChild(_startButton);
		addListeners();
	}

	public function close():void {
		removeListeners();
		_container.removeChild(view);
		_container.removeChild(_startButton);
	}

	public function getDashboard():Dashboard { return _dashboard; }

	private function initObjects():void {
		_dashboard = new Dashboard(thisView.dashboard);
		_startButton = new StartButtonView();
		_startButton.x = 100;
		_startButton.y = 100;
	}

	private function addListeners():void {
		_startButton.addEventListener(MouseEvent.MOUSE_OVER, onStartBtnMouseOver);
		_startButton.addEventListener(MouseEvent.MOUSE_OUT, onStartBtnMouseOut);
		_startButton.addEventListener(MouseEvent.CLICK, onStartBtnClick);
	}
	private function removeListeners():void {
		_startButton.removeEventListener(MouseEvent.MOUSE_OVER, onStartBtnMouseOver);
		_startButton.removeEventListener(MouseEvent.MOUSE_OUT, onStartBtnMouseOut);
		_startButton.removeEventListener(MouseEvent.CLICK, onStartBtnClick);
	}

	private function onStartBtnMouseOver(event:MouseEvent):void {
		TweenMax.to(event.target, .3, {glowFilter:{color: 0xfaa43c, alpha : 1, blurX: 20, blurY: 20, inner: true}});
	}
	private function onStartBtnMouseOut(event:MouseEvent):void {
		TweenMax.to(event.target, .3, {glowFilter:{alpha : 0, blurX: 20, blurY: 20, inner: true}});
	}
	private function onStartBtnClick(event:MouseEvent):void {
		dispatchEvent(new SceneEvent(SceneEvent.SWITCH_ME, this));
	}

	private function get thisView():MenuView { return view as MenuView; }

}
}