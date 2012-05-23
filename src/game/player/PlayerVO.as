/**
 * Created by : Dmitry
 * Date: 4/27/12
 * Time: 9:25 AM
 */
package game.player {
public class PlayerVO {
	public var userId:String;
	public var color:uint;

	public function PlayerVO(userId:String, color:uint):void {
		super();
		this.userId = userId;
		this.color = color;
	}
}
}
