/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 7/8/12
 * Time: 11:41 PM
 * To change this template use File | Settings | File Templates.
 */
package game.board.view {
import flash.display.Sprite;

public class CPointView extends Sprite {
	public function CPointView(positive:Boolean):void {
		super();
		init(positive); //TODO debug time only
		//super.addChild(positive ? Main.POSITIVE_COUNTABLE_POINT : NEGATIVE_COUNTABLE_POINT);
	}

	private function init(positive:Boolean):void {
		super.graphics.beginFill(positive ? 0xffffff : 0, .5);
		super.graphics.drawCircle(0, 0, 10);
	}
}
}
