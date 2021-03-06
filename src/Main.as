package {
import controller.GameProcess;

import core.enum.WindowsENUM;

import core.window.WindowManager;

import flash.display.Sprite;

import game.iface.window.EndGameWindow;

import game.iface.window.RequestWindow;
import game.staticModel.MatchState;
import game.manager.RequestManager;

import game.staticModel.UserState;

import mainMenu.MainMenu;

import rpc.GameRpc;

import scene.SceneController;

	[SWF(width=600, height=480, frameRate=40)]
	public class Main extends Sprite {
		public static const WIDTH:Number = 600;
		public static const HEIGHT:Number = 480;

		[Embed (source="../imgs/woodboard_3.png" )] public static const BOARD_VIEW:Class;
		[Embed (source="../imgs/white_stone.png" )] public static const WHITE_STONE_VIEW:Class;
		[Embed (source="../imgs/black_stone.png" )] public static const BLACK_STONE_VIEW:Class;
		[Embed (source="../imgs/white_base_stone.png" )] public static const WHITE_BASIC_STONE_VIEW:Class;
		[Embed (source="../imgs/black_base_stone.png" )] public static const BLACK_BASIC_STONE_VIEW:Class;
		private var _sceneController:SceneController;
		
		public function Main() {
			GameRpc.instance.init("localhost", 8080);
			GameRpc.instance.authorize(onAuthorizeComplete);
			addScenes();
			registerWindows();
			RequestManager.instance.init();
			MatchState.instance.init();
		}

		private function onAuthorizeComplete(result:Object):void {
			UserState.instance.init(result.toString());
		}

		private function addScenes():void {
			_sceneController = new SceneController();
			const menuScene:MainMenu = new MainMenu(this);
			const gameScene:GameProcess = new GameProcess(this);
			_sceneController.addScene(menuScene, true);
			_sceneController.addScene(gameScene);
			_sceneController.addSceneDependence(menuScene, gameScene, true);
		}

		private function registerWindows():void {
			WindowManager.instance.layer = this;
			WindowManager.instance.registerWindow(WindowsENUM.REQUEST_WINDOW, new RequestWindow());
			WindowManager.instance.registerWindow(WindowsENUM.End_WINDOW, new EndGameWindow());
		}
	}
}