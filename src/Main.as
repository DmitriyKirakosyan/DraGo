package {
import controller.GameController;

import flash.display.Sprite;

import game.player.PlayerVO;

import game.staticModel.MatchInfo;

import mainMenu.MainMenu;

import scene.SceneController;

	[SWF(width=600, height=480, frameRate=40)]
	public class Main extends Sprite {
		public static const WIDTH:Number = 600;
		public static const HEIGHT:Number = 480;

		private var _sceneController:SceneController;
		
		public function Main() {
			addScenes();
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
	}
}