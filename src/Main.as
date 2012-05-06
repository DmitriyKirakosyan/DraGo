package {
import controller.GameController;

import core.enum.WindowsENUM;

import core.window.WindowManager;

import flash.display.Sprite;

import game.iface.window.RequestWindow;
import game.manager.RequestManager;

import game.player.PlayerVO;

import game.staticModel.MatchInfo;
import game.staticModel.UserState;

import mainMenu.MainMenu;

import rpc.GameRpc;

import scene.SceneController;

	[SWF(width=600, height=480, frameRate=40)]
	public class Main extends Sprite {
		public static const WIDTH:Number = 600;
		public static const HEIGHT:Number = 480;

		private var _sceneController:SceneController;
		
		public function Main() {
			GameRpc.instance.init("localhost", 8080);
			GameRpc.instance.authorize(onAuthorizeComplete);
			addScenes();
			registerWindows();
			new RequestManager();
		}

		private function onAuthorizeComplete(result:Object):void {
			UserState.instance.init(result.toString());
		}

		private function addScenes():void {
			_sceneController = new SceneController();
			const menuScene:MainMenu = new MainMenu(this);
			const gameScene:GameController = new GameController(this);
			_sceneController.addScene(menuScene, true);
			_sceneController.addScene(gameScene);
			_sceneController.addSceneDependence(menuScene, gameScene, true);

			MatchInfo.instance.whitePlayer = new PlayerVO(true);
			MatchInfo.instance.blackPlayer = new PlayerVO(true);
		}

		private function registerWindows():void {
			WindowManager.instance.layer = this;
			WindowManager.instance.registerWindow(WindowsENUM.REQUEST_WINDOW, new RequestWindow());
		}
	}
}