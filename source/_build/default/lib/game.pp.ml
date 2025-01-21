Caml1999M028����            +lib/game.ml����  Q�    C)  B�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,inline_tests�@�@@����'enabled��.<command-line>A@A�A@H@@��A@@�A@I@@@@�@@����������������,library-name�@�@@����,libnewtonoid��A@A�A@M@@��A@N@@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@�����@������"()��+lib/game.mlBAAA@@@��������3Ppx_inline_test_lib'Runtime5set_lib_and_partition@@��@���(@@@��@��� @@@@@@@@��������(Iterator��"BAF�#BAN@�@@A�&@@�'@��������$Type��1COT�2COX@�@@A��5COO@@�@��������%Brick��ADY^�BDYc@�@@A��EDYY@@�@��������)Gamestate��QEdi�REdr@�@@A��UEdd@@�@���@�����*game_hello��aHuy�bHu C@�@@@��@@����"()��lHu D�mHu F@@�@@@������-print_endline��wI J L�xI J Y@�@@@��@���9Bienvenue dans Newtonoid!���I J [��I J t@@���I J Z��I J u@@@@�@@@�A@@@���Huu@@�@������)BrickInit���K w ~��K w �@�����@�����$rows���L � ���L � �@�@@@���!5@���L � ���L � �@@@@���L � �@@�@���@�����$cols���M � ���M � �@�@@@���!8@���M � ���M � �@@@@���M � �@@�@���@�����+brick_width���N � ���N � �@�@@@���#80.@���N � ���N � �@@@@���N � �@@�@���@�����,brick_height���O � ���O � �@�@@@���#20.@���O � ���O � �@@@@���O � �@@�@���@�����'spacing���P � ���P � �@�@@@���#10.@��P � ��P � �@@@@��P � �@@�@@��K w ��Q � �@@@��K w w@�@������$Game��S � ��S � �@�����@�����-initial_state��&U$*�'U$7@�@@@������$ball��1V<@�2V<D@������#pos��;WIO�<WIR@������$400.@��EWIV�FWIZ@@@����$300.@��MWI\�NWI`@@@@��PWIU�QWIa@��@@@����#vel��ZXci�[Xcl@������%400.0@��dXcp�eXcu@@@����%-400.@��lXcw�mXc|@@@@��oXco�pXc}@��@@@����&radius��yY��zY�@���"5.@���Y���Y�@@@@@���V<G��Z��@@@����&paddle���[����[��@������#pos���\����\��@������$350.@���\����\��@@@����#50.@���\����\��@@@@���\����\��@��@@@����#dim���]����]��@������$100.@���]����]��@@@����#10.@���]����]��@@@@���]����]��@��@@@����#vel���^����^��@������"0.@���^����^��@@@����"0.@���^����^��@@@@���^����^��@��@@@@@���[����_��@@@����&bricks���`����`��@�������(BrickSet+create_grid��`��`�@�@@@��@�����)BrickInit$rows��`��`�%@�@@@��@�����)BrickInit$cols��`�&�`�4@�@@@��@�����)BrickInit+brick_width��(`�5�)`�J@�@@@��@�����)BrickInit,brick_height��5`�K�6`�a@�@@@��@�����)BrickInit'spacing��B`�b�C`�s@�@@@@�E@@@����%score��Lauy�Mau~@���!0@��Sau��Tau�@@@����%lives��[b���\b��@���!3@��bb���cb��@@@����'running��jc���kc��@����$true��rc���sc��@@�@@@@@��vU$:�wd��@@@@��yU$&@@�@���@�����1create_game_state���h����h�@�@@@��@@������'mouse_x���h���h�@�@@@��@���h���h�@@@@���h���h�@��@@@��@�����(paddle_x���i��i%@�@@@������#max���i(��i+@�@@@��@���#10.@���i,��i/@@@��@������#min���i1��i4@�@@@��@������"-.���i;��i=@�@@@��@���$790.@���i6��i:@@@��@���$100.@���i>��iB@@@@���i5��iC@��@@@��@����'mouse_x���iD��iK@�@@@@���i0��iL@��7@@@@�N@@@@��i@@������&paddle��kio�kiu@������#pos��ki��ki�@�������(paddle_x�� ki��!ki�@�@@@����#50.@��)ki��*ki�@@@@��,ki��-ki�@��@@@@�������-initial_state��9kiz�:ki�@�@@@��&paddle��@ki��Aki�@�
@@@��Dkix�Eki�@@@@�����-initial_state��MjPV�NjPc@�@@@��QjPT�Rl��@@@�R@@@��A@@@��Vh��@@�@���@�����5handle_wall_collision��bo���co��@�@@@��@@���$ball��lo���mo� @�@@@��@��������!x��{p�|p@�@@@����!y���p��p@�@@@@���p��p@��@@@������$ball���p��p@�@@@��#pos���p��p@�
@@@@���p@@��@��������"vx���q )��q +@�@@@����"vy���q -��q /@�@@@@���q (��q 0@��@@@������$ball���q 3��q 7@�@@@��#vel���q 8��q ;@�
@@@@���q $@@��@�����&new_vx���r?G��r?M@�@@@��������"||���r?\��r?^@�@@@��@������"<=���r?U��r?W@�@@@��@����!x���r?S� r?T@�@@@��@���#10.@��	r?X�
r?[@@@@�@@@��@������">=��r?a�r?c@�@@@��@����!x��!r?_�"r?`@�@@@��@���$790.@��+r?d�,r?h@@@@�@@@@�0@@@������#~-.��7r?n�8r?p@�@@@��@����"vx�
�Br?r@�@@@@�@@@�����"vx��Lr?x�Mr?z@�@@@��Pr?P@@@@��Rr?C@@��@�����&new_vy��\s~��]s~�@�@@@�����������hs~��is~�@�@@@��@������"<=��us~��vs~�@�@@@��@����!y���s~���s~�@�@@@��@���#10.@���s~���s~�@@@@�@@@��@������">=���s~���s~�@�@@@��@����!y���s~���s~�@�@@@��@���$590.@���s~���s~�@@@@�@@@@�0@@@������#~-.���s~���s~�@�@@@��@����"vy�
��s~�@�@@@@�@@@�����"vy���s~���s~�@�@@@���s~�@@@@���s~�@@������#vel���t����t��@�������&new_vx���t����t��@�@@@�����&new_vy���t����t��@�@@@@���t����t��@��@@@@�����$ball�� t���t��@�@@@��t���t��@@@�4@@@��@@@�9@@@�k@@@��A@@@��o��@@�	@���@�����7handle_paddle_collision��v���v�@�@@@��@@���$ball��"v��#v�	@�@@@��@@�����&paddle��.v��/v�@�@@@����+etat_racket��7v��8v�@@�@@@��;v�
�<v�@@@��@��������"px��Iw"/�Jw"1@�@@@����"py��Rw"3�Sw"5@�@@@@��Vw".�Ww"6@��@@@������&paddle��bw"9�cw"?@�@@@��#pos��iw"@�jw"C@�
@@@@��mw"*@@��@��������"pw��zxGT�{xGV@�@@@����"ph���xGX��xGZ@�@@@@���xGS��xG[@��@@@������&paddle���xG^��xGd@�@@@��#dim���xGe��xGh@�
@@@@���xGO@@��@��������"bx���yly��yl{@�@@@����"by���yl}��yl@�@@@@���ylx��yl�@��@@@������$ball���yl���yl�@�@@@��#pos���yl���yl�@�
@@@@���ylt@@��@��������"vx���z����z��@�@@@����"vy���z����z��@�@@@@���z����z��@��@@@������$ball���z����z��@�@@@��#vel���z����z��@�
@@@@�� z��@@��������"<=��|���|��@�@@@��@����"by��|���|��@�@@@��@������"+.��#|���$|��@�@@@��@����"py��.|���/|��@�@@@��@����"ph��9|���:|��@�@@@@�@@@@�(@@@��������"&&��H4J�I4L@�@@@��@������">=��U4D�V4F@�@@@��@����"by��`4A�a4C@�@@@��@����"py��k4G�l4I@�@@@@�@@@��@������1��x4\�y4^@�@@@��@������"<=���4P��4R@�@@@��@����"by���4M��4O@�@@@��@������z���4V��4X@�@@@��@����"py���4S��4U@�@@@��@����"ph���4Y��4[@�@@@@�@@@@�'@@@��@������y���4h��4j@�@@@��@������">=���4b��4d@�@@@��@����"bx���4_��4a@�@@@��@����"px���4e��4g@�@@@@�@@@��@������"<=���4n��4p@�@@@��@����"bx���4k��4m@�@@@��@��������4t�	4v@�@@@��@����"px��4q�4s@�@@@��@����"pw��4w�4y@�@@@@�@@@@�'@@@@�L@@@@��@@@@��@@@��@�����*relative_x��/ @��0 @�@�@@@������"/.��: @��; @�@�@@@��@������q��F @��G @�@�@@@��@����"bx��Q @��R @�@�@@@��@����"px��\ @��] @�@�@@@@��` @��a @�@��@@@��@����"pw��l @��m @�@�@@@@�@@@@��q @�@@��@�����%angle��{ A���| A��@�@@@������"*.��� A��� A�@�@@@��@���������� A���� A��@�@@@��@����*relative_x��� A���� A��@�@@@��@���#0.5@��� A� �� A�@@@@��� A���� A�@��@@@��@���#1.0@��� A��� A�@@@@�@@@@��� A��@@��@�����%speed��� B<L�� B<Q@�@@@������$sqrt��� B<T�� B<X@�@@@��@���������� B<c�� B<e@�@@@��@������"*.��� B<]�� B<_@�@@@��@����"vx��� B<Z�� B<\@�@@@��@����"vx��� B<`�� B<b@�@@@@�@@@��@������"*.�� B<i� B<k@�@@@��@����"vy�� B<f� B<h@�@@@��@����"vy��! B<l�" B<n@�@@@@�@@@@��& B<Y�' B<o@��8@@@@�]@@@@��, B<H@@��@�����&new_vx��6 Cs��7 Cs�@�@@@������"*.��A Cs��B Cs�@�@@@��@����%speed��L Cs��M Cs�@�@@@��@������#sin��Y Cs��Z Cs�@�@@@��@����%angle��d Cs��e Cs�@�@@@@�@@@@�@@@@��j Cs@@��@�����&new_vy��t D���u D��@�@@@������)abs_float�� D���� D��@�@@@��@������"*.��� D���� D��@�@@@��@����%speed��� D���� D��@�@@@��@������#cos��� D���� D��@�@@@��@����%angle��� D���� D��@�@@@@�@@@@��� D���� D��@��!@@@@�:@@@@��� D��@@������#vel��� E'�� E*@�������&new_vx��� E.�� E4@�@@@�����&new_vy��� E6�� E<@�@@@@��� E-�� E=@��@@@@�����$ball��� E�� E!@�@@@��� E�� E?@@@�4@@@��@@@��@@@�8@@@��@@@�����$ball��� GO[�� GO_@�@@@���4>@@@�����$ball��	 Imy�	 Im}@�@@@��		|��@@@�@@@�=@@@�o@@@��@@@��	A@@��
A@@@��	v��@@�@���@�����/check_ball_lost��	 K��	 K�@�@@@��@@���$ball��	' K��	( K�@�@@@������!<��	2 L���	3 L��@�@@@��@������#snd��	? L���	@ L��@�@@@��@������$ball��	L L���	M L��@�@@@��#pos��	S L���	T L��@�
@@@@�@@@��@������#snd��	a L���	b L��@�@@@��@��������-initial_state��	p L���	q L��@�@@@��&paddle��	w L���	x L��@�
@@@��#pos��	~ L���	 L��@�@@@@�!@@@@�D@@@�]A@@@��	� K�@@�@���@�����,update_state��	� O	"	(�	� O	"	4@�@@@��@@���"dt��	� O	"	5�	� O	"	7@�@@@��@@������'mouse_x��	� O	"	9�	� O	"	@@�@@@��@��	� O	"	B�	� O	"	C@@@@��	� O	"	8�	� O	"	D@��@@@��@@���%state��	� O	"	E�	� O	"	J@�@@@��������#not��	� P	M	T�	� P	M	W@�@@@��@������%state��	� P	M	X�	� P	M	]@�@@@��'running��	� P	M	^�	� P	M	e@�
@@@@�@@@����%state��	� Q	l	r�	� Q	l	w@�@@@���@�����,new_paddle_x��	� T	�	��	� T	�	�@�@@@������#max��	� T	�	��
  T	�	�@�@@@��@���#10.@��
	 T	�	��

 T	�	�@@@��@������#min��
 T	�	��
 T	�	�@�@@@��@������L��
! T	�	��
" T	�	�@�@@@��@���$790.@��
+ T	�	��
, T	�	�@@@��@���$100.@��
4 T	�	��
5 T	�	�@@@@��
7 T	�	��
8 T	�	�@��@@@��@����'mouse_x��
C T	�	��
D T	�	�@�@@@@��
G T	�	��
H T	�	�@��6@@@@�M@@@@��
M T	�	�@@��@�����*new_paddle��
W U	�	��
X U	�	�@�@@@������#pos��
b U	�
�
c U	�
@�������,new_paddle_x��
m U	�
�
n U	�
(@�@@@����#50.@��
v U	�
*�
w U	�
-@@@@��
y U	�
�
z U	�
.@��@@@@�������%state��
� U	�
�
� U	�
@�@@@��&paddle��
� U	�
	�
� U	�
@�
@@@��
� U	�
�
� U	�
0@@@@��
� U	�	�@@��@��������!x��
� X
Z
e�
� X
Z
f@�@@@����!y��
� X
Z
h�
� X
Z
i@�@@@@��
� X
Z
d�
� X
Z
j@��@@@��������%state��
� X
Z
m�
� X
Z
r@�@@@��$ball��
� X
Z
s�
� X
Z
w@�
@@@��#pos��
� X
Z
x�
� X
Z
{@�@@@@��
� X
Z
`@@��@��������"vx��
� Y

��
� Y

�@�@@@����"vy��
� Y

��
� Y

�@�@@@@��
� Y

��
� Y

�@��@@@��������%state��
� Y

��
� Y

�@�@@@��$ball��
� Y

��
� Y

�@�
@@@��#vel�� Y

�� Y

�@�@@@@�� Y

�@@��@�����,new_ball_pos�� Z
�
�� Z
�
�@�@@@������������ Z
�
��  Z
�
�@�@@@��@����!x��* Z
�
��+ Z
�
�@�@@@��@������"*.��7 Z
�
��8 Z
�
�@�@@@��@����"vx��B Z
�
��C Z
�
�@�@@@��@����"dt��M Z
�
��N Z
�
�@�@@@@�@@@@�(@@@�������8��Z Z
�
��[ Z
�
�@�@@@��@����!y��e Z
�
��f Z
�
�@�@@@��@������"*.��r Z
�
��s Z
�
�@�@@@��@����"vy��} Z
�
��~ Z
�
�@�@@@��@����"dt��� Z
�
��� Z
�
�@�@@@@�@@@@�(@@@@��� Z
�
��� Z
�
�@��h	@@@@��� Z
�
�@@��@�����(new_ball��� [
�
��� [
�
�@�@@@������#pos��� [
��� [
�@����,new_ball_pos��� [
��� [
�@�@@@@�������%state��� [
�
��� [
�
�@�@@@��$ball��� [
�
��� [
�@�
@@@��� [
�
��� [
�@@@@��� [
�
�@@��@�����0ball_after_walls��� ^HR�� ^Hb@�@@@������5handle_wall_collision��� ^He�� ^Hz@�@@@��@����(new_ball��� ^H{�� ^H�@�@@@@�@@@@��� ^HN@@��@�����1ball_after_paddle��� _���� _��@�@@@������7handle_paddle_collision�� _��� _��@�@@@��@����0ball_after_walls�� _��� _��@�@@@��@����*new_paddle�� _��� _��@�@@@@�@@@@�� _��@@��@�����0colliding_bricks��) b�* b+@�@@@�������(BrickSet4get_colliding_bricks��6 b.�7 bK@�@@@��@������%state��C bL�D bQ@�@@@��&bricks��J bR�K bX@�
@@@��@����1ball_after_paddle��U bY�V bj@�@@@@�#@@@@��Z b@@��@�����*new_bricks��d cnx�e cn�@�@@@�������(BrickSet-update_bricks��q cn��r cn�@�@@@��@������%state��~ cn�� cn�@�@@@��&bricks��� cn��� cn�@�
@@@��@����1ball_after_paddle��� cn��� cn�@�@@@@�#@@@@��� cnt@@��@�����/score_increment��� d���� d��@�@@@������!*��� d���� d��@�@@@��@�������$List&length��� d���� d��@�@@@��@����0colliding_bricks��� d���� d��@�@@@@�@@@��@���"10@��� d���� d��@@@@�@@@@��� d��@@��@�����1ball_after_bricks��� f 
�� f @�@@@��������!>��� gG�� gH@�@@@��@�������$List&length��� g*�� g5@�@@@��@����0colliding_bricks�� g6� gF@�@@@@�@@@��@���!0@�� gI� gJ@@@@�@@@������#vel�� hPs� hPv@��@��������"vx��( hP~�) hP�@�@@@����"vy��1 hP��2 hP�@�@@@@��5 hP}�6 hP�@��@@@������1ball_after_paddle��A hP��B hP�@�@@@��#vel��H hP��I hP�@�
@@@@��L hPy@@�������"vx��V hP��W hP�@�@@@�������#~-.��b hP��c hP�@�@@@��@����"vy�
�m hP�@�@@@@�@@@@��q hP��r hP�@��@@@�*@@@@�����1ball_after_paddle��} hP\�~ hPm@�@@@��� hPZ�� hP�@@@�����1ball_after_paddle��� j���� j��@�@@@��� g'@@@@��� f @@��������i��� n0S�� n0T@�@@@��@������#snd��� n09�� n0<@�@@@��@������1ball_after_bricks��� n0=�� n0N@�@@@��#pos��� n0O�� n0R@�
@@@@�@@@��@������#snd��� n0U�� n0X@�@@@��@��������-initial_state��� n0Y�� n0f@�@@@��&paddle��� n0g�� n0m@�
@@@��#pos��� n0n�� n0q@�@@@@�!@@@@�D@@@��������"<=��� ow��� ow�@�@@@��@������%state�� ow�� ow�@�@@@��%lives��	 ow��
 ow�@�
@@@��@���!1@�� ow�� ow�@@@@�@@@������'running�� p��� p��@����%false��& p���' p��@@�@@@@�����%state��0 p���1 p��@�@@@��4 p���5 p��@@@�������$ball��? s���@ s��@������#pos��I s�
�J s�@��������-initial_state��U s��V s�@�@@@��$ball��\ s��] s�"@�
@@@��#pos��c s�#�d s�&@�@@@����#vel��l s�(�m s�+@��������-initial_state��x s�.�y s�;@�@@@��$ball�� s�<�� s�@@�
@@@��#vel��� s�A�� s�D@�@@@@�������%state��� s���� s��@�@@@��$ball��� s� �� s�@�
@@@��� s���� s�F@@@����%lives��� tHT�� tHY@������!-��� tHh�� tHi@�@@@��@������%state��� tH\�� tHa@�@@@��%lives��� tHb�� tHg@�
@@@��@���!1@��� tHj�� tHk@@@@�@@@����%score��� umy�� um~@������!+��� um��� um�@�@@@��@������%state��� um��� um�@�@@@��%score��� um��� um�@�
@@@��@����/score_increment��� um��  um�@�@@@@�@@@@�����%state��
 r��� r��@�@@@�� r��� um�@@@�� ow@@@���������"==�� v��� v��@�@@@��@�������$List&length��, v���- v��@�@@@��@������%state��9 v���: v��@�@@@��&bricks��@ v���A v��@�
@@@@�@@@��@���!0@��K v���L v��@@@@�"@@@������'running��V w���W w��@����8��] w���^ w��@@�@@@@�����%state��g w���h w��@�@@@��k w���l w��@@@�������$ball��v z#�w z'@����1ball_after_bricks��~ z*� z;@�@@@����&paddle��� {=G�� {=M@����*new_paddle��� {=P�� {=Z@�@@@����&bricks��� |\f�� |\l@����*new_bricks��� |\o�� |\y@�@@@����%score��� }{��� }{�@������Ӱ�� }{��� }{�@�@@@��@������%state��� }{��� }{�@�@@@��%score��� }{��� }{�@�
@@@��@����/score_increment��� }{��� }{�@�@@@@�@@@@�����%state��� y�� y@�@@@��� y�� }{�@@@��� v��@@@��� n06@@@�W@@@�@@@�T@@@��	@@@��
@@@��@@@�#@@@�[@@@��@@@�"@@@�]@@@��@@@��� P	M	Q@@@�9A@@�EA@@�\A@@@��� O	"	$@@�@���@�����)game_loop�� ���� ���@�@@@��@@������� ���� ���@@�@@@��@�����"dt�� ���� ���@�@@@������"/.��% ���& ��@�@@@��@���"1.@��/ ����0 �� @@@��@���#60.@��8 ���9 ��@@@@�@@@@��< ���@@�������$Flux&unfold��G ��H �(@�@@@��@��@@���%state��S �)4�T �)9@�@@@����$Some��\ �=E�] �=I@��������%state��h �=K�i �=P@�@@@�������,update_state��t �=R�u �=^@�@@@��@����"dt�� �=_�� �=a@�@@@��@���������"|>��� �=�� �=�@�@@@��@������#fst��� �=c�� �=f@�@@@��@�������(Graphics)mouse_pos��� �=h�� �=z@�@@@��@����J��� �={�� �=}@@�@@@@��� �=g�� �=~@��@@@@�"@@@��@����,float_of_int��� �=��� �=�@�@@@@�.@@@��������� �=��� �=�@@�@@@@��� �=b�� �=�@��<@@@��@����%state��� �=��� �=�@�@@@@�p@@@@��� �=J�� �=�@���@@@��@@@��� �)/�� �=�@���� �)0
@@@��@������1create_game_state��� ����� ���@�@@@��@������$400.@�� ���� ���@@@������� ���� ���@@�@@@@�� ���� ���@��@@@@�� ���� ���@��"@@@@��@@@��@@@�A@@@��  ���@@�	@@��#S � ��$ ���@@@��&S � �@�@���@������3�A@@@������0)unset_lib@@��@���T@@@@@@@@@