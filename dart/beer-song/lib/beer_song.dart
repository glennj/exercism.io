// experimenting with inheritance
import 'package:beer_song/beverage_song.dart';

class BeerSong extends BeverageSong {
  BeerSong() : super(liquid: 'beer', where: 'on the wall');
}
