package mainMenu {
import com.greensock.TweenMax;

import controller.ViewController;

import flash.display.Sprite;
import flash.events.MouseEvent;

import game.events.MatchManagerEvent;

import game.manager.MatchManager;

import scene.IScene;
import scene.SceneEvent;

public class MainMenu extends ViewController implements IScene {
	private var _playersPanel:PlayersPanel;
	private var _container:Sprite;
	private var _startButton:StartButtonView;

	public function MainMenu(container:Sprite) {
		super(new MenuView());
		_container = container;
		initObjects();
		MatchManager.instance.addEventListener(MatchManagerEvent.GAME_STARTED, onGameStarted);
	}

	public function open():void {
		view.width = Main.WIDTH;
		view.height = Main.HEIGHT;
		_container.addChild(view);
		_container.addChild(_startButton);
		_container.addChild(_playersPanel);
		addListeners();
	}

	public function close():void {
		removeListeners();
		_container.removeChild(view);
		_container.removeChild(_startButton);
		_container.removeChild(_playersPanel);
	}

	private function initObjects():void {
		thisView.removeChild(thisView.dashboard);
		_playersPanel = new PlayersPanel();
		_playersPanel.x = Main.WIDTH - _playersPanel.width;
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

	private function onGameStarted(event:MatchManagerEvent):void {
		dispatchEvent(new SceneEvent((SceneEvent.SWITCH_ME), this));
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