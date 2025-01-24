Caml1999M028����            +lib/game.ml����  ��  �  s�  q`�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,inline_tests�@�@@����'enabled��.<command-line>A@A�A@H@@��A@@�A@I@@@@�@@����������������,library-name�@�@@����,libnewtonoid��A@A�A@M@@��A@N@@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@�����@������"()��+lib/game.mlA@@A@@@��������3Ppx_inline_test_lib'Runtime5set_lib_and_partition@@��@���(@@@��@��� @@@@@@@@��������(Iterator��"A@E�#A@M@�@@A�&@@�'@��������$Type��1BNS�2BNW@�@@A��5BNN@@�@��������%Brick��ACX]�BCXb@�@@A��ECXX@@�@��������)Gamestate��QDch�RDcq@�@@A��UDcc@@�@��������$Menu��aErw�bEr{@�@@A��eErr@@�@��������%Power��qF| A�rF| F@�@@A��uF||@@�@���@�����*game_hello���H H L��H H V@�@@@��@@����"()���H H W��H H Y@@�@@@������-print_endline���I ] _��I ] l@�@@@��@���9Bienvenue dans Newtonoid!���I ] n��I ] �@@���I ] m��I ] �@@@@�@@@�A@@@���H H H@@�@������)BrickInit���K � ���K � �@�����@�����$rows���L � ���L � �@�@@@���!7@���L � ���L � �@@@@���L � �@@�@���@�����$cols���M � ���M � �@�@@@���"14@���M � ���M � �@@@@���M � �@@�@���@�����+brick_width���N � ���N � �@�@@@���#40.@���N � ���N � �@@@@���N � �@@�@���@�����,brick_height��O � ��O � �@�@@@���#10.@��O � ��O � �@@@@��O � �@@�@���@�����'spacing��P � ��P � �@�@@@���#15.@��%P � ��&P � �@@@@��(P � �@@�@���@�����+power_ratio��4Q ��5Q �@�@@@���#0.1@��<Q ��=Q �@@@@��?Q �@@�@@��BK � ��CR@@@��EK � �@�@������$Game��OT#�PT'@�����@�����-initial_state��]VPV�^VPc@�@@@��@�����!b��iWgo�jWgp@�@@@�������(BrickSet+create_grid��vWgs�wWg�@�@@@��@�����)BrickInit$rows���Wg���Wg�@�@@@��@�����)BrickInit$cols���Wg���Wg�@�@@@��@�����)BrickInit+brick_width���Wg���Wg�@�@@@��@�����)BrickInit,brick_height���Wg���Wg�@�@@@��@�����)BrickInit'spacing���Wg���Wg�@�@@@��@�����)BrickInit+power_ratio���Wg���Wg�@�@@@@�R@@@@���Wgk@@������$ball���Y��Y
@������#pos���Z��Z@������$400.@���Z��Z @@@����$300.@���Z"��Z&@@@@���Z��Z'@��@@@����#vel���[)/��[)2@������%400.0@��[)6�[);@@@����%-400.@��[)=�[)B@@@@��[)5�[)C@��@@@����&radius��\EK�\EQ@���"5.@��!\ET�"\EV@@@@@��$Y�%]W\@@@����&paddle��,^^b�-^^h@������#pos��6_ms�7_mv@������$350.@��@_mz�A_m~@@@����#50.@��H_m��I_m�@@@@��K_my�L_m�@��@@@����#dim��U`���V`��@������$100.@��_`���``��@@@����#10.@��g`���h`��@@@@��j`���k`��@��@@@����#vel��ta���ua��@������"0.@��~a���a��@@@����"0.@���a����a��@@@@���a����a��@��@@@@@���^^k��b��@@@����&bricks���c����c��@����!b���c����c��@�@@@����%power���d����d��@�������(PowerSet2create_from_bricks���d����d��@�@@@��@����!b���d����d��@�@@@@�@@@����+actif_power���e����e�@����"[]���e���e�
@@�@@@����*power_time���f��f@���#2.0@���f��f @@@����%score���g"&��g"+@���!0@���g".��g"/@@@����%lives���h15��h1:@���#300@���h1=��h1@@@@����'running��iBF�iBM@����$true��iBP�iBT@@�@@@@@��X� �jUX@@@�L@@@@��VPR@@�@���@�����%norme��"lZ`�#lZe@�@@@��@@������!x��/lZg�0lZh@�@@@����!y��8lZj�9lZk@�@@@@��<lZf�=lZl@��@@@������$sqrt��HlZo�IlZs@�@@@��@������"+.��UlZ{�VlZ}@�@@@��@������"*.��blZv�clZx@�@@@��@����!x��mlZt�nlZu@�@@@��@����!x��xlZy�ylZz@�@@@@�@@@��@������"*.���lZ���lZ�@�@@@��@����!y���lZ~��lZ@�@@@��@����!y���lZ���lZ�@�@@@@�@@@@�X��lZ�@��7@@@@�]@@@�jA@@@���lZ\@@�@���@�����1create_game_state���o����o��@�@@@��@@������'mouse_x���o����o��@�@@@��@���o����o��@@@@���o����o��@��@@@��@�����(paddle_x���p����p� @�@@@������#max���p���p�@�@@@��@���#10.@���p���p�
@@@��@������#min���p���p�@�@@@��@������"-.��p��p�@�@@@��@���$790.@��p��p�@@@��@���$100.@��p��p�@@@@��p��p�@��@@@��@����'mouse_x��&p��'p�&@�@@@@��*p��+p�'@��7@@@@�N@@@@��0p��@@������&paddle��9rDJ�:rDP@������#pos��CrDo�DrDr@�������(paddle_x��NrDv�OrD~@�@@@����#50.@��WrD��XrD�@@@@��ZrDu�[rD�@��@@@@�������-initial_state��grDU�hrDb@�@@@��&paddle��nrDc�orDi@�
@@@��rrDS�srD�@@@@�����-initial_state��{q+1�|q+>@�@@@��q+/��s��@@@�R@@@��A@@@���o��@@�@���@�����5handle_wall_collision���v����v��@�@@@��@@���$ball���v����v��@�@@@��@��������!x���w����w��@�@@@����!y���w����w��@�@@@@���w����w��@��@@@������$ball���w����w��@�@@@��#pos���w����w��@�
@@@@���w��@@��@��������"vx���x���x�@�@@@����"vy���x���x�
@�@@@@���x���x�@��@@@������$ball���x���x�@�@@@��#vel���x���x�@�
@@@@���x��@@��@�����&new_vx��y"�	y(@�@@@��������"||��y7�y9@�@@@��@������"<=��"y0�#y2@�@@@��@����!x��-y.�.y/@�@@@��@���#10.@��7y3�8y6@@@@�@@@��@������">=��Dy<�Ey>@�@@@��@����!x��Oy:�Py;@�@@@��@���$790.@��Yy?�ZyC@@@@�@@@@�0@@@������#~-.��eyI�fyK@�@@@��@����"vx�
�pyM@�@@@@�@@@�����"vx��zyS�{yU@�@@@��~y+@@@@���y@@��@�����&new_vy���zYa��zYg@�@@@������������zYv��zYx@�@@@��@������"<=���zYo��zYq@�@@@��@����!y���zYm��zYn@�@@@��@���#10.@���zYr��zYu@@@@�@@@��@������">=���zY{��zY}@�@@@��@����!y���zYy��zYz@�@@@��@���$590.@���zY~��zY�@@@@�@@@@�0@@@������#~-.���zY���zY�@�@@@��@����"vy�
��zY�@�@@@@�@@@�����"vy���zY���zY�@�@@@���zYj@@@@��zY]@@������#vel��
{���{��@�������&new_vx��{���{��@�@@@�����&new_vy��{��� {��@�@@@@��#{���${��@��@@@@�����$ball��.{���/{��@�@@@��2{���3{��@@@�4@@@��@@@�9@@@�k@@@��A@@@��:v��@@�	@���@�����7handle_paddle_collision��F}���G}��@�@@@��@@���$ball��P}���Q}��@�@@@��@@�����&paddle��\}���]}��@�@@@����+etat_racket��e}���f}��@@�@@@��i}���j}��@@@��@��������"px��w~�
�x~�@�@@@����"py���~���~�@�@@@@���~�	��~�@��@@@������&paddle���~���~�@�@@@��#pos���~���~�@�
@@@@���~�@@��@��������"pw���"/��"1@�@@@����"ph���"3��"5@�@@@@���".��"6@��@@@������&paddle���"9��"?@�@@@��#dim���"@��"C@�
@@@@���"*@@��@��������"bx��� @GT�� @GV@�@@@����"by��� @GX�� @GZ@�@@@@��� @GS�� @G[@��@@@������$ball��� @G^�� @Gb@�@@@��#pos��� @Gc�� @Gf@�
@@@@��� @GO@@��@��������"vx��
 Ajw� Ajy@�@@@����"vy�� Aj{� Aj}@�@@@@�� Ajv� Aj~@��@@@������$ball��# Aj��$ Aj�@�@@@��#vel��* Aj��+ Aj�@�
@@@@��. Ajr@@��������"<=��9 C���: C��@�@@@��@����"by��D C���E C��@�@@@��@���������P C���Q C��@�@@@��@����"py��[ C���\ C��@�@@@��@����"ph��f C���g C��@�@@@@�@@@@�'@@@��������"&&��u F%�v F'@�@@@��@������">=��� F�� F!@�@@@��@����"by��� F�� F@�@@@��@����"py��� F"�� F$@�@@@@�@@@��@������1��� F7�� F9@�@@@��@������"<=��� F+�� F-@�@@@��@����"by��� F(�� F*@�@@@��@������u��� F1�� F3@�@@@��@����"py��� F.�� F0@�@@@��@����"ph��� F4�� F6@�@@@@�@@@@�'@@@��@������y��� FC�� FE@�@@@��@������">=��� F=�� F?@�@@@��@����"bx�� F:� F<@�@@@��@����"px�� F@� FB@�@@@@�@@@��@������"<=�� FI� FK@�@@@��@����"bx��) FF�* FH@�@@@��@��������5 FO�6 FQ@�@@@��@����"px��@ FL�A FN@�@@@��@����"pw��K FR�L FT@�@@@@�@@@@�'@@@@�L@@@@��@@@@��@@@��@�����*relative_x��\ GZj�] GZt@�@@@������"/.��g GZ��h GZ�@�@@@��@������p��s GZ{�t GZ}@�@@@��@����"bx��~ GZx� GZz@�@@@��@����"px��� GZ~�� GZ�@�@@@@��� GZw�� GZ�@��@@@��@����"pw��� GZ��� GZ�@�@@@@�@@@@��� GZf@@��@�����%angle��� H���� H��@�@@@������"*.��� H���� H��@�@@@��@���������� H���� H��@�@@@��@����*relative_x��� H���� H��@�@@@��@���#0.5@��� H���� H��@@@@��� H���� H��@��@@@��@���#1.0@��� H���� H��@@@@�@@@@��� H��@@��@�����%speed��� I'�� I,@�@@@������$sqrt��� I/�� I3@�@@@��@���������	 I>�	 I@@�@@@��@������"*.��	 I8�	 I:@�@@@��@����"vx��	 I5�	  I7@�@@@��@����"vx��	* I;�	+ I=@�@@@@�@@@��@������"*.��	8 ID�	9 IF@�@@@��@����"vy��	C IA�	D IC@�@@@��@����"vy��	N IG�	O II@�@@@@�@@@@��	S I4�	T IJ@��8@@@@�]@@@@��	Y I#@@��@�����&new_vx��	c JN^�	d JNd@�@@@������"*.��	n JNm�	o JNo@�@@@��@����%speed��	y JNg�	z JNl@�@@@��@������#sin��	� JNp�	� JNs@�@@@��@����%angle��	� JNt�	� JNy@�@@@@�@@@@�@@@@��	� JNZ@@��@�����&new_vy��	� K}��	� K}�@�@@@������)abs_float��	� K}��	� K}�@�@@@��@������"*.��	� K}��	� K}�@�@@@��@����%speed��	� K}��	� K}�@�@@@��@������#cos��	� K}��	� K}�@�@@@��@����%angle��	� K}��	� K}�@�@@@@�@@@@��	� K}��	� K}�@��!@@@@�:@@@@��	� K}�@@������#vel��	� L�	�	� L�	@�������&new_vx��	� L�		�	� L�	@�@@@�����&new_vy��
 L�	�
 L�	@�@@@@��
	 L�	�

 L�	@��@@@@�����$ball��
 L���
 L��@�@@@��
 L���
 L�	@@@�4@@@��@@@��@@@�8@@@��@@@�����$ball��
& N	*	6�
' N	*	:@�@@@��
* F@@@�����$ball��
2 P	H	T�
3 P	H	X@�@@@��
6 C��@@@�
@@@�<@@@�n@@@��@@@��	A@@��
A@@@��
>}��@@�@���@�����/check_ball_lost��
J R	Z	`�
K R	Z	o@�@@@��@@���$ball��
T R	Z	p�
U R	Z	t@�@@@������!<��
_ S	w	��
` S	w	�@�@@@��@������#snd��
l S	w	��
m S	w	�@�@@@��@������$ball��
y S	w	��
z S	w	�@�@@@��#pos��
� S	w	��
� S	w	�@�
@@@@�@@@��@������#snd��
� S	w	��
� S	w	�@�@@@��@��������-initial_state��
� S	w	��
� S	w	�@�@@@��&paddle��
� S	w	��
� S	w	�@�
@@@��#pos��
� S	w	��
� S	w	�@�@@@@�!@@@@�D@@@�]A@@@��
� R	Z	\@@�@���@�����,update_state��
� V	�
�
� V	�
@�@@@��@@���"dt��
� V	�
�
� V	�
@�@@@��@@������'mouse_x��
� V	�
�
� V	�
@�@@@��@��
� V	�
�
� V	�
@@@@��
� V	�
�
� V	�
@��@@@��@@���%state��
� V	�
 �
� V	�
%@�@@@��������#not��
� W
(
/�
� W
(
2@�@@@��@������%state�� W
(
3� W
(
8@�@@@��'running��
 W
(
9� W
(
@@�
@@@@�@@@����%state�� X
G
M� X
G
R@�@@@���@�����,new_paddle_x��! [
�
��" [
�
�@�@@@������#max��, [
�
��- [
�
�@�@@@��@���#10.@��6 [
�
��7 [
�
�@@@��@������#min��B [
�
��C [
�
�@�@@@��@������K��N [
�
��O [
�
�@�@@@��@���$790.@��X [
�
��Y [
�
�@@@��@���$100.@��a [
�
��b [
�
�@@@@��d [
�
��e [
�
�@��@@@��@����'mouse_x��p [
�
��q [
�
�@�@@@@��t [
�
��u [
�
�@��6@@@@�M@@@@��z [
�
�@@��@�����*new_paddle��� \
�
��� \
�
�@�@@@������#pos��� \
�
��� \
�
�@�������,new_paddle_x��� \
�
��� \
�@�@@@����#50.@��� \
��� \
�@@@@��� \
�
��� \
�	@��@@@@�������%state��� \
�
��� \
�
�@�@@@��&paddle��� \
�
��� \
�
�@�
@@@��� \
�
��� \
�@@@@��� \
�
�@@��@��������!x��� _5@�� _5A@�@@@����!y��� _5C�� _5D@�@@@@��� _5?�� _5E@��@@@��������%state��� _5H�� _5M@�@@@��$ball��� _5N�� _5R@�
@@@��#pos��� _5S�� _5V@�@@@@��� _5;@@��@��������"vx�� `Ze�	 `Zg@�@@@����"vy�� `Zi� `Zk@�@@@@�� `Zd� `Zl@��@@@��������%state��# `Zo�$ `Zt@�@@@��$ball��* `Zu�+ `Zy@�
@@@��#vel��1 `Zz�2 `Z}@�@@@@��5 `Z`@@��@�����,new_ball_pos��? a���@ a��@�@@@������������L a���M a��@�@@@��@����!x��W a���X a��@�@@@��@������"*.��d a���e a��@�@@@��@����"vx��o a���p a��@�@@@��@����"dt��z a���{ a��@�@@@@�@@@@�(@@@�������	3��� a���� a��@�@@@��@����!y��� a���� a��@�@@@��@������"*.��� a���� a��@�@@@��@����"vy��� a���� a��@�@@@��@����"dt��� a���� a��@�@@@@�@@@@�(@@@@��� a���� a��@��h	@@@@��� a��@@��@�����(new_ball��� b���� b��@�@@@������#pos��� b���� b��@����,new_ball_pos��� b���� b��@�@@@@�������%state��� b���� b��@�@@@��$ball��� b���� b��@�
@@@��� b���� b��@@@@��� b��@@��@�����0ball_after_walls�� e#-� e#=@�@@@������5handle_wall_collision�� e#@� e#U@�@@@��@����(new_ball�� e#V� e#^@�@@@@�@@@@�� e#)@@��@�����1ball_after_paddle��& fbl�' fb}@�@@@������7handle_paddle_collision��1 fb��2 fb�@�@@@��@����0ball_after_walls��< fb��= fb�@�@@@��@����*new_paddle��G fb��H fb�@�@@@@�@@@@��L fbh@@��@�����0colliding_bricks��V i���W i�@�@@@�������(BrickSet4get_colliding_bricks��c i�	�d i�&@�@@@��@������%state��p i�'�q i�,@�@@@��&bricks��w i�-�x i�3@�
@@@��@����1ball_after_paddle��� i�4�� i�E@�@@@@�#@@@@��� i��@@��@�����*new_bricks��� jIS�� jI]@�@@@�������(BrickSet-update_bricks��� jI`�� jIv@�@@@��@������%state��� jIw�� jI|@�@@@��&bricks��� jI}�� jI�@�
@@@��@����1ball_after_paddle��� jI��� jI�@�@@@@�#@@@@��� jIO@@��@�����/score_increment��� k���� k��@�@@@������!*��� k���� k��@�@@@��@�������$List&length��� k���� k��@�@@@��@����0colliding_bricks��� k���� k��@�@@@@�@@@��@���"10@��� k���� k��@@@@�@@@@��  k��@@��@�����0colliding_powers��
 n&� n6@�@@@�������(PowerSet4get_colliding_powers�� n9� nV@�@@@��@������%state��$ nW�% n\@�@@@��%power��+ n]�, nb@�
@@@��@������%state��8 nc�9 nh@�@@@��&paddle��? ni�@ no@�
@@@@�,@@@@��D n"@@��@�����)new_power��N os}�O os�@�@@@�������(PowerSet-update_powers��[ os��\ os�@�@@@��@������%state��h os��i os�@�@@@��&paddle��o os��p os�@�
@@@��@����"dt��z os��{ os�@�@@@��@������%state��� os��� os�@�@@@��%power��� os��� os�@�
@@@��@����*new_bricks��� os��� os�@�@@@@�B@@@@��� osy@@��@�����-activatePower��� r���� r��@�@@@�������$List#map��� r���� r��@�@@@��@��@@���%power��� r���� r��@�@@@�������%Power(get_type��� r��� r�@�@@@��@����%power��� r��� r�@�@@@@�@@@��� r���� r�@���� r��	@@@��@����0colliding_powers��� r��� r�)@�@@@@�:@@@@��� r��@@��@�����4power_activated_tabs��� t.8�� t.L@�@@@�������$List#map�� t.O� t.W@�@@@��@��@@���%typeP�� t.]� t.b@�@@@�������%typeP�� t.g�  t.l@�@@@����#0.0@��( t.n�) t.q@@@@��+ t.f�, t.r@��@@@��0 t.X�1 t.s@���4 t.Y	@@@��@����-activatePower��= t.t�> t.�@�@@@@�:@@@@��B t.4@@��@�����1ball_after_bricks��L v���M v��@�@@@��������!>��Y w���Z w��@�@@@��@�������$List&length��h w���i w��@�@@@��@����0colliding_bricks��s w���t w��@�@@@@�@@@��@���!0@��~ w��� w��@@@@�@@@������#vel��� x���� x��@��@��������"vx��� x��� x�@�@@@����"vy��� x��� x�	@�@@@@��� x��� x�
@��@@@������1ball_after_paddle��� x��� x�@�@@@��#vel��� x��� x�"@�
@@@@��� x��@@�������"vx��� x�'�� x�)@�@@@�������#~-.��� x�+�� x�-@�@@@��@����"vy�
�� x�/@�@@@@�@@@@��� x�&�� x�0@��@@@�*@@@@�����1ball_after_paddle��� x���� x��@�@@@��� x���� x�2@@@�����1ball_after_paddle��� z@J�� z@[@�@@@��� w��@@@@��� v��@@��@�����<ball_after_activating_powers��	 }fp�
 }f�@�@@@���������$List#mem�� ~��� ~��@�@@@��@�����%Brick'SpeedUp��% ~���& ~��@@�@@@��@����-activatePower��0 ~���1 ~��@�@@@@�@@@��@��������"vx��@ ���A ��@�@@@����"vy��I ���J ��@�@@@@��M ���N ��@��@@@������1ball_after_bricks��Y ���Z ��@�@@@��#vel��` ���a ��@�
@@@@��d ��@@������#vel��m ���n ��@���������"*.��z ���{ �� @�@@@��@���#1.5@��� ���� ��@@@��@����"vx��� ��!�� ��#@�@@@@�@@@�������"*.��� ��)�� ��+@�@@@��@���#1.5@��� ��%�� ��(@@@��@����"vy��� ��,�� ��.@�@@@@�@@@@��� ���� ��/@��4@@@@�����1ball_after_bricks��� ����� ��@�@@@��� ����� ��0@@@�b@@@�����1ball_after_bricks��� �=E�� �=V@�@@@��� ~��@@@@��� }fl@@��@�����=paddle_after_activating_power��� �ak�� �a�@�@@@���������$List#mem��� ����� ���@�@@@��@�����%Brick-EnlargePaddle��� ����� ���@@�@@@��@����-activatePower�� ���� ���@�@@@@�@@@��@��������$dimx�� ���� ���@�@@@����$dimy�� ���� ���@�@@@@��! ����" ���@��@@@������*new_paddle��- ����. ���@�@@@��#dim��4 ����5 ���@�
@@@@��8 ���@@������#dim��A ���B ��@���������"*.��N ���O ��@�@@@��@����$dimx��Y ���Z ��@�@@@��@���#1.5@��c �� �d ��#@@@@�@@@�����$dimy��m ��%�n ��)@�@@@@��q ���r ��*@��@@@@�����*new_paddle��| ���} ��@�@@@��� �� �� ��+@@@�K@@@�����*new_paddle��� �9C�� �9M@�@@@��� ���@@@@��� �ag@@��@�����:ball_after_power_dissipate��� �Xb�� �X|@�@@@��@�����.ballSpeedPower��� ����� ���@�@@@�������$List&filter��� ����� ���@�@@@��@��@@���$elem��� ����� ���@�@@@������!=��� ����� ���@�@@@��@������#fst��� ����� ���@�@@@��@����$elem��� ����� ���@�@@@@�@@@��@�����%Brick'SpeedUp��� ����� ���@@�@@@@�@@@��� ����� ���@���� ���	@@@��@����4power_activated_tabs�� ���� ���@�@@@@�S@@@@�� ���@@����������� ��� ��@�@@@��@�������$List&length��  ����! ���@�@@@��@����.ballSpeedPower��+ ����, ��@�@@@@�@@@��@���!0@��6 ���7 ��@@@@�@@@��@�����/disspated_power��B �%�C �4@�@@@�������$List&filter��O �7�P �B@�@@@��@��@@���$elem��[ �H�\ �L@�@@@��������e �Y�f �Z@�@@@��@������#snd��r �P�s �S@�@@@��@����$elem��} �T�~ �X@�@@@@�@@@��@������%state��� �[�� �`@�@@@��*power_time��� �a�� �k@�
@@@@�$@@@��� �C�� �l@���� �D	@@@��@����.ballSpeedPower��� �m�� �{@�@@@@�Y@@@@��� �!@@��������[��� ���� ��@�@@@��@�������$List&length��� ���� ��@�@@@��@����/disspated_power��� ���� ��@�@@@@�@@@��@���!0@��� ���� ��@@@@�@@@��@��������"vx��� ����� ���@�@@@����"vy��� ����� ���@�@@@@��� ����� ���@��@@@������<ball_after_activating_powers��  ���� ���@�@@@��#vel�� ���� ���@�
@@@@�� ���@@������#vel�� ��� ��"@���������"/.��! ��)�" ��+@�@@@��@����"vx��, ��&�- ��(@�@@@��@������"*.��9 ��1�: ��3@�@@@��@���#1.5@��C ��-�D ��0@@@��@������)abs_float��O ��4�P ��=@�@@@��@������%float��\ ��?�] ��D@�@@@��@������!-��i ��a�j ��b@�@@@��@�������$List&length��x ��F�y ��Q@�@@@��@����.ballSpeedPower��� ��R�� ��`@�@@@@�@@@��@�������$List&length��� ��c�� ��n@�@@@��@����/disspated_power��� ��o�� ��~@�@@@@�@@@@��� ��E�� ��@��/@@@@��� ��>�� ���@��P@@@@�^@@@@��� ��,�� ���@��o	@@@@��@@@�������"/.��� ����� ���@�@@@��@����"vy��� ����� ���@�@@@��@������"*.��� ����� ���@�@@@��@���#1.5@��� ����� ���@@@��@������)abs_float��� ����� ���@�@@@��@������%float��� ����� ���@�@@@��@��������� ���� ���@�@@@��@�������$List&length�� ���� ���@�@@@��@����.ballSpeedPower�� ���� ���@�@@@@�@@@��@�������$List&length��- ����. ���@�@@@��@����/disspated_power��8 ����9 ���@�@@@@�@@@@��= ����> ���@��/@@@@��B ����C ���@��O@@@@�]@@@@��H ����I ���@��n	@@@@��@@@@��N ��%�O ���@��&	@@@@�����<ball_after_activating_powers��Y ����Z ��@�@@@��] ����^ ���@@@�U@@@�����<ball_after_activating_powers��g ����h ��@�@@@��k ��@@@��@@@�����<ball_after_activating_powers��t �&0�u �&L@�@@@��x ���@@@�s@@@@��{ �X^@@��@�����<paddle_after_power_dissipate��� �Wa�� �W}@�@@@��@�����0paddleLargePower��� ����� ���@�@@@�������$List&filter��� ����� ���@�@@@��@��@@���$elem��� ����� ���@�@@@��������� ����� ���@�@@@��@������#fst��� ����� ���@�@@@��@����$elem��� ����� ���@�@@@@�@@@��@�����%Brick-EnlargePaddle��� ����� ���@@�@@@@�@@@��� ����� ���@���� ���	@@@��@����4power_activated_tabs��� ����� ���@�@@@@�R@@@@��� ���@@������������ ���� ��@�@@@��@�������$List&length��
 ���� ��@�@@@��@����0paddleLargePower�� ��� ��@�@@@@�@@@��@���!0@��  ���! ��@@@@�@@@��@�����/disspated_power��, �"0�- �"?@�@@@�������$List&filter��9 �"B�: �"M@�@@@��@��@@���$elem��E �"S�F �"W@�@@@���������O �"d�P �"e@�@@@��@������#snd��\ �"[�] �"^@�@@@��@����$elem��g �"_�h �"c@�@@@@�@@@��@������%state��u �"f�v �"k@�@@@��*power_time��| �"l�} �"v@�
@@@@�$@@@��� �"N�� �"w@���� �"O	@@@��@����0paddleLargePower��� �"x�� �"�@�@@@@�Y@@@@��� �",@@��������E��� ����� ���@�@@@��@�������$List&length��� ����� ���@�@@@��@����/disspated_power��� ����� ���@�@@@@�@@@��@���!0@��� ����� ���@@@@�@@@��@��������$dimx��� ����� ���@�@@@����$dimy��� ����� ���@�@@@@��� ����� ���@��@@@������=paddle_after_activating_power��� ����� ���@�@@@��#dim��� ����� ���@�
@@@@��� ���@@������#dim��� �2�� �5@���������"/.�� �>� �@@�@@@��@����$dimx�� �9� �=@�@@@��@������"*.��# �F�$ �H@�@@@��@���#1.5@��- �B�. �E@@@��@������%float��9 �I�: �N@�@@@��@������ݰ�E �m�F �n@�@@@��@�������$List&length��T �P�U �[@�@@@��@����0paddleLargePower��_ �\�` �l@�@@@@�@@@��@�������$List&length��o �o�p �z@�@@@��@����/disspated_power��z �{�{ ��@�@@@@�@@@@�� �O�� ��@��/@@@@�K@@@@��� �A�� ��@��\	@@@@�t@@@�����$dimy��� ���� ��@�@@@@��� �8�� ��@���@@@@�����=paddle_after_activating_power��� ��� �,@�@@@��� ��� ��@@@��@@@�����=paddle_after_activating_power��� ����� ���@�@@@��� ���@@@�!@@@�����=paddle_after_activating_power��� ����� ��@�@@@��� ���@@@��@@@@��� �W]@@��@�����/new_actif_power��� ��� �&@�@@@�������$List*filter_map��� �)�� �8@�@@@��@��@@������%elem1��� �?�� �D@�@@@����%elem2��� �F�� �K@�@@@@��� �>�� �L@��@@@����������� �Y� �Z@�@@@��@����%elem2�� �S� �X@�@@@��@������%state�� �[� �`@�@@@��*power_time��! �a�" �k@�
@@@@�@@@����$None��+ �q�, �u@@�@@@�����$Some��5 �{�6 �@��������%elem1��A ���B ��@�@@@����������L ���M ��@�@@@��@����%elem2��W ���X ��@�@@@��@����"dt��b ���c ��@�@@@@�@@@@��g ���h ��@��*@@@�7@@@��m �P@@@��o �9�p ��@���s �:@@@��@������%state��~ ��� ��@�@@@��+actif_power��� ���� ��@�
@@@@��@@@@��� �@@��������6��� �/�� �0@�@@@��@������#snd��� �
�� �@�@@@��@������<ball_after_activating_powers��� ��� �*@�@@@��#pos��� �+�� �.@�
@@@@�@@@��@������#snd��� �1�� �4@�@@@��@��������-initial_state��� �5�� �B@�@@@��&paddle��� �C�� �I@�
@@@��#pos��� �J�� �M@�@@@@�!@@@@�D@@@��������"<=��� �Sj�� �Sl@�@@@��@������%state��� �S^�� �Sc@�@@@��%lives�� �Sd� �Si@�
@@@��@���!1@�� �Sm� �Sn@@@@�@@@������'running�� ���� ���@����%false��  ����! ���@@�@@@@�����%state��* ����+ ���@�@@@��. �t~�/ ���@@@���@�����+direction_x��: ����; ���@�@@@��������D ��F�E ��H@�@@@��@������M��P ��(�Q ��*@�@@@��@������#fst��] ���^ ��@�@@@��@������=paddle_after_activating_power��j ���k ��#@�@@@��#pos��q ��$�r ��'@�
@@@@�@@@��@������#fst�� ��+�� ��.@�@@@��@��������-initial_state��� ��/�� ��<@�@@@��$ball��� ��=�� ��A@�
@@@��#pos��� ��B�� ��E@�@@@@�!@@@@�D@@@��@������"/.��� ��o�� ��q@�@@@��@������#fst��� ��I�� ��L@�@@@��@������=paddle_after_activating_power��� ��M�� ��j@�@@@��#dim��� ��k�� ��n@�
@@@@�@@@��@���#2.0@��� ��r�� ��u@@@@�"@@@@�~@@@@��� ���@@��@�����+direction_y��� �y��� �y�@�@@@���������� �y��� �y�@�@@@��@������#snd��� �y��� �y�@�@@@��@������=paddle_after_activating_power��
 �y�� �y�@�@@@��#pos�� �y�� �y�@�
@@@@�@@@��@������#snd�� �y��  �y�@�@@@��@��������-initial_state��. �y��/ �y�@�@@@��$ball��5 �y��6 �y�@�
@@@��#pos��< �y��= �y�@�@@@@�!@@@@�D@@@@��B �y�@@��@�����$norm��L ����M ���@�@@@������%norme��W ����X ���@�@@@��@�������+direction_x��e ����f ���@�@@@�����+direction_y��o ���p ��@�@@@@��s ����t ��@��@@@@�!@@@@��y ���@@��@�����*velocity_x��� ��� �'@�@@@������"*.��� �E�� �G@�@@@��@������$sqrt��� �*�� �.@�@@@��@������"*.��� �<�� �>@�@@@��@������"*.��� �4�� �6@�@@@��@���#2.0@��� �0�� �3@@@��@���$400.@��� �7�� �;@@@@�@@@��@���$400.@��� �?�� �C@@@@��� �/�� �D@��@@@@�?@@@��@������"/.��� �U�� �W@�@@@��@����+direction_x��� �I�� �T@�@@@��@����$norm��� �X�� �\@�@@@@��� �H�� �]@��@@@@�h@@@@�� �@@��@�����*velocity_y�� �am� �aw@�@@@������"*.�� �a�� �a�@�@@@��@������$sqrt��& �az�' �a~@�@@@��@������"*.��3 �a��4 �a�@�@@@��@������"*.��@ �a��A �a�@�@@@��@���#2.0@��J �a��K �a�@@@��@���$400.@��S �a��T �a�@@@@�@@@��@���$400.@��] �a��^ �a�@@@@��` �a�a �a�@��@@@@�?@@@��@������"/.��o �a��p �a�@�@@@��@����+direction_y��z �a��{ �a�@�@@@��@����$norm��� �a��� �a�@�@@@@��� �a��� �a�@��@@@@�h@@@@��� �ai@@������$ball��� ����� ���@������#pos��� ����� ���@��������-initial_state��� ����� ��@�@@@��$ball��� ���� ��@�
@@@��#pos��� ���� ��@�@@@����#vel��� ��� �@�������*velocity_x��� �#�� �-@�@@@�����*velocity_y��� �/�� �9@�@@@@��� �"�� �:@��@@@@�������%state��� ����� ���@�@@@��$ball��� ����� ���@�
@@@��� ����� �<E@@@����%lives��� �GO�� �GT@��������� �Gc� �Gd@�@@@��@������%state�� �GW� �G\@�@@@��%lives�� �G]� �Gb@�
@@@��@���!1@��% �Ge�& �Gf@@@@�@@@����%score��. �hp�/ �hu@������!+��8 �h��9 �h�@�@@@��@������%state��E �hx�F �h}@�@@@��%score��L �h~�M �h�@�
@@@��@����/score_increment��W �h��X �h�@�@@@@�@@@@�����%state��b ����c ���@�@@@��f ����g ���@@@��@@@�f@@@��@@@�*@@@��@@@��n �S[@@@���������"==��z ����{ ���@�@@@��@�������$List&length��� ����� ���@�@@@��@������%state��� ����� ���@�@@@��&bricks��� ����� ���@�
@@@@�@@@��@���!0@��� ����� ���@@@@�"@@@������'running��� ���� ��@�������� ���� ��$@@�@@@@�����%state��� ��
�� ��@�@@@��� ����� �'2@@@�������$ball��� �S]�� �Sa@����:ball_after_power_dissipate��� �Sd�� �S~@�@@@����&paddle��� ����� ���@����<paddle_after_power_dissipate��� ����� ���@�@@@����&bricks��� ����� ���@����*new_bricks��� ����� ���@�@@@����%power�� ���� ���@����)new_power�� ���� ���@�@@@����+actif_power�� ���� ��@����/new_actif_power�� ���  ��@�@@@����%score��( � �) �%@���������1 �4�2 �5@�@@@��@������%state��> �(�? �-@�@@@��%score��E �.�F �3@�
@@@��@����/score_increment��P �6�Q �E@�@@@@�@@@@�����%state��[ �>H�\ �>M@�@@@��_ �>F�` �G@@@��b ���@@@��d �@@@��@@@��@@@��@@@�
�	@@@��
@@@�l@@@�*@@@�}@@@��@@@�+@@@�p@@@��@@@��@@@�'@@@�X@@@�~@@@��@@@�B@@@�}@@@��@@@� @@@��{ W
(
,@@@��A@@��A@@��A@@@��� V	�	�!@@�"@���@�����)game_loop��� �w}�� �w�@�@@@��@@������� �w��� �w�@@�@@@��@�����"dt��� ����� ���@�@@@������"/.��� ����� ���@�@@@��@���"1.@��� ����� ���@@@��@���#60.@��� ����� ���@@@@�@@@@��� ���@@�������$Flux&unfold��� ����� ���@�@@@��@��@@���%state��� ����� ���@�@@@����$Some��� ����� ���@��������%state��� ����� ���@�@@@�������,update_state��� ����� ���@�@@@��@����"dt�� ���� ���@�@@@��@����������� ��,� ��.@�@@@��@������"|>��# ���$ ��@�@@@��@������#fst��0 ����1 ��@�@@@��@�������(Graphics)mouse_pos��? ���@ ��@�@@@��@�������I ���J ��@@�@@@@��M ���N ��@��@@@@�"@@@��@����,float_of_int��Z ���[ ��*@�@@@@��^ ����_ ��+@��2@@@��@������"/.��l ��D�m ��F@�@@@��@������#fst��y ��/�z ��2@�@@@��@��������%state��� ��3�� ��8@�@@@��&paddle��� ��9�� ��?@�
@@@��#dim��� ��@�� ��C@�@@@@�!@@@��@���#2.0@��� ��G�� ��J@@@@�+@@@@�G@@@��������� ��L�� ��Q@@�@@@@��� ����� ��R@��U@@@��@����%state��� ��S�� ��X@�@@@@��@@@@��� ����� ��Y@���@@@��@@@��� ����� ��Z@���� ���
@@@��@������1create_game_state��� �[b�� �[s@�@@@��@������$400.@��� �[u�� �[y@@@�����˰�� �[{�� �[�@@�@@@@��� �[t�� �[�@��@@@@��� �[a�� �[�@��"@@@@�)@@@�5@@@�dA@@@��� �wy@@�	@@���T*�� ���@@@��T@�@���@�������A@@@������)unset_lib@@��@���/@@@@@@@@@