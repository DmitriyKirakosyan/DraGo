/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 11:58 PM
 */
package game.request {
public class RequestVO {
	public var owner:String;
	public var userFor:String;
	public var isNew:Boolean;

	public function RequestVO(owner:String, userFor:String, isNew:Boolean):void {
		this.owner = owner;
		this.userFor = userFor;
		this.isNew = isNew;
	}
}
}
