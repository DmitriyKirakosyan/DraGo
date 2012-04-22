/**
 * Created by : Dmitry
 * Date: 4/22/12
 * Time: 10:26 PM
 */
package controller {
import flash.display.Sprite;
import flash.events.EventDispatcher;
import flash.events.MouseEvent;

import scene.IScene;
import scene.SceneEvent;

public class GameController extends EventDispatcher implements IScene {
	private var _container:Sprite;
	private var _gameContainer:Sprite;

	public function GameController(container:Sprite) {
		_container = container;
		initObjects();
	}

	public function open():void {
		_container.addChild(_gameContainer);
		addListeners();
	}
	public function close():void {
	 	removeListeners();
		_container.removeChild(_gameContainer);
	}

	/* Internal functions */

	private function initObjects():void {
		_gameContainer = new Sprite();
		_gameContainer.graphics.beginFill(0xf33daa);
		_gameContainer.graphics.drawRect(0, 0, Main.WIDTH, Main.HEIGHT);
		_gameContainer.graphics.endFill();
	}

	private function addListeners():void {
		_gameContainer.addEventListener(MouseEvent.CLICK, onClick);
	}
	private function removeListeners():void {
		_gameContainer.removeEventListener(MouseEvent.CLICK, onClick);
	}

	private function onClick(event:MouseEvent):void {
		dispatchEvent(new SceneEvent(SceneEvent.SWITCH_ME, this));
	}
}
}
