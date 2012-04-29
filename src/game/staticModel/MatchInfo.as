/**
 * Created by : Dmitry
 * Date: 4/26/12
 * Time: 11:41 PM
 */
package game.staticModel {
import game.player.PlayerVO;

public class MatchInfo {
	private static var _instance:MatchInfo;

	private var _whitePlayer:PlayerVO;
	private var _blackPlayer:PlayerVO;

	public static function get instance():MatchInfo {
		if (!_instance) { _instance = new MatchInfo(); }
		return _instance;
	}

	public function MatchInfo():void {
	}

	public function get whitePlayer():PlayerVO {
		return _whitePlayer;
	}
	public function get blackPlayer():PlayerVO {
		return _blackPlayer;
	}

	public function set whitePlayer(value:PlayerVO):void {
		_whitePlayer = value;
	}
	public function set blackPlayer(value:PlayerVO):void {
		_blackPlayer = value;
	}
}
}
