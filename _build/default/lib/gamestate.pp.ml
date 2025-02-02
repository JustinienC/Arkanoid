Caml1999M028����            0lib/gamestate.ml����  F�  �  7^  5������1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,inline_tests�@�@@����'enabled��.<command-line>A@A�A@H@@��A@@�A@I@@@@�@@����������������,library-name�@�@@����,libnewtonoid��A@A�A@M@@��A@N@@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@�����@������"()��0lib/gamestate.mlCXXA@@@��������3Ppx_inline_test_lib'Runtime5set_lib_and_partition@@��@���(@@@��@��� @@@@@@@@��������$Ball��"CX]�#CXa@�@@A�&@@�'@��������%Brick��1Dbg�2Dbl@�@@A��5Dbb@@�@��������&Paddle��AEmr�BEmx@�@@A��EEmm@@�@��������&Config��QFy~�RFy D@�@@A��UFyy@@�@��������(Quadtree��aG E J�bG E R@�@@A��eG E E@@�@��������%Bonus��qH S X�rH S ]@�@@A��uH S S@@�@������)Gamestate��J _ f��J _ o@�����A�    �+game_status���L � ���L � �@@@��Р'Playing���M � ���M � �@�@@���M � �@@�Р&Paused���N � ���N � �@�@@���N � �@@�Р(GameOver���O � ���O � �@�@@���O � �@@�Р-LevelComplete���P � ���P � �@�@@���P � �@@�Р)NextLevel���Q � ���Q � �@�@@���Q � �@@�Р(Starting���R ���R �
@�@@���R � @@@A@@���L � �@@�@���A�    �!t���e����e��@@@��Р$ball���f����f��@@�����$Ball!t���f����f��@@�@@@���f��@@�Р&paddle���g����g��@@�����&Paddle!t���g����g��@@�@@@���g��@@�Р&bricks��h���h��@@����$list��h���h��@������%Brick!t��h���h��@@�@@@@�@@@��h��@@�Р%score��#i���$i��@@����#int��+i���,i��@@�@@@��/i��@@�Р%lives��5j���6j��@@����#int��=j���>j��@@�@@@��Aj��@@�Р%level��Gk��Hk�@@����#int��Ok�	�Pk�@@�@@@��Sk�@@�Р&status��Yl�Zl@@����+game_status��al�bl%@@�@@@��el&@@�Р+last_update��km'+�lm'6@@����%float��sm'8�tm'=@@�@@@��wm'>@@�Р(quadtree��}o\`�~o\h@@�����(Quadtree$node���o\r��o\@������%Brick!t���o\j��o\q@@�@@@@�@@@���o\�@@�Р+bonus_items���p����p��@@����$list���p����p��@������%Bonus!t���p����p��@@�@@@@�@@@���p��@@�Р.active_effects���q����q��@@����$list���q����q�@���������%Brick,bonus_effect���q����q��@@�@@@�����%float���q����q��@@�@@@@�@@@@���q��@@@�'��q�@@�Р3active_ball_effects���r	��r@@����$list���r;��r?@���������%Brick,bonus_effect���r��r1@@�@@@�����%float��r4�	r9@@�@@@@�@@@@��r@@@�'�r@@@@A@@��e���sRU@@�@���A�    �,brick_layout�� @`g� @`s@@@��Р+brick_count��$ Axz�% Ax�@@����#int��, Ax��- Ax�@@�@@@��0 Ax�@@�Р$cols��6 B���7 B��@@����#int��> B���? B��@@�@@@��B B��@@�Р$rows��H C���I C��@@����#int��P C���Q C��@@�@@@��T C��@@�Р'spacing��Z D���[ D��@@�������%float��e D���f D��@@�@@@�����%float��o D���p D��@@�@@@@�@@@��t D��@@@A@@��v @`b�w E��@@�@���@�����6calculate_brick_layout��� QRV�� QRl@�@@@��@@���+brick_width��� QRm�� QRx@�@@@��@@���,brick_height��� QRy�� QR�@�@@@��@@���-screen_bounds��� QR��� QR�@�@@@��@@���'spacing��� QR��� QR�@�@@@�  ��@�����2horizontal_spacing��� R���� R��@�@@@����'spacing��� R���� R��@�@@@@��� R��@@��@�����0vertical_spacing��� S���� S��@�@@@����'spacing��� S���� S��@�@@@@��� S��@@��@�����������%x_min��� T���� T�	@�@@@����%y_min��� T�	�� T�	@�@@@@��� T���� T�	@��@@@�������%x_max�� T�	� T�	@�@@@����%y_max�� T�	� T�	@�@@@@�� T�	� T�	@��@@@@�� T��� T�	@��#@@@����-screen_bounds��# T�	 �$ T�	-@�@@@@��' T��@@��@�����,screen_width��1 U	1	7�2 U	1	C@�@@@������"-.��< U	1	L�= U	1	N@�@@@��@����%x_max��G U	1	F�H U	1	K@�@@@��@����%x_min��R U	1	O�S U	1	T@�@@@@�@@@@��W U	1	3@@��@�����-screen_height��a V	X	^�b V	X	k@�@@@������0��k V	X	t�l V	X	v@�@@@��@����%y_max��v V	X	n�w V	X	s@�@@@��@����%y_min��� V	X	w�� V	X	|@�@@@@�@@@@��� V	X	Z@@��@�����0available_height��� X	�	��� X	�	�@�@@@������"/.��� X	�	��� X	�	�@�@@@��@����-screen_height��� X	�	��� X	�	�@�@@@��@���"2.@��� X	�	��� X	�	�@@@@�@@@@��� X	�	�@@��@�����$cols��� [
3
9�� [
3
=@�@@@������,int_of_float��� [
3
@�� [
3
L@�@@@��@������"/.��� [
3
s�� [
3
u@�@@@��@������"+.��� [
3
\�� [
3
^@�@@@��@����,screen_width��� [
3
O�� [
3
[@�@@@��@����2horizontal_spacing��� [
3
_�� [
3
q@�@@@@��� [
3
N�� [
3
r@��@@@��@������(��
 [
3
�� [
3
�@�@@@��@����+brick_width�� [
3
w� [
3
�@�@@@��@����2horizontal_spacing��  [
3
��! [
3
�@�@@@@��$ [
3
v�% [
3
�@��@@@@��) [
3
M�* [
3
�@��0@@@@�e@@@@��/ [
3
5@@��@�����$rows��9 ]
�
��: ]
�
�@�@@@������,int_of_float��D ]
�
��E ]
�
�@�@@@��@������"/.��Q ]
�'�R ]
�)@�@@@��@������{��] ]
��^ ]
�@�@@@��@����0available_height��h ]
��i ]
�@�@@@��@����0vertical_spacing��s ]
��t ]
�%@�@@@@��w ]
� �x ]
�&@��@@@��@���������� ]
�8�� ]
�:@�@@@��@����,brick_height��� ]
�+�� ]
�7@�@@@��@����0vertical_spacing��� ]
�;�� ]
�K@�@@@@��� ]
�*�� ]
�L@��@@@@��� ]
�
��� ]
�M@��0@@@@�d@@@@��� ]
�
�@@������+brick_count��� `X\�� `Xg@������!*��� `Xo�� `Xp@�@@@��@����$cols��� `Xj�� `Xn@�@@@��@����$rows��� `Xq�� `Xu@�@@@@�@@@����$cols��� aw{�� aw@�����@@@����$rows��� b���� b��@�����@@@����'spacing��� c���� c��@�������2horizontal_spacing�� c��� c��@�@@@�����0vertical_spacing�� c��� c��@�@@@@�� c��� c��@��@@@@@�� _TV� d��@@@�n@@@��@@@�e@@@��@@@��@@@��@@@�@@@@�X	@@@����,brick_layout��$ QR��% QR�@@�@@@��( QR�A@@�A@@��A@@��A@@��A@@@��. QRR@@�@���@�����1create_brick_grid��: t���; t��@�@@@��@@���$rows��D t���E t��@�@@@��@@���$cols��N t���O t��@�@@@��@@���+brick_width��X t���Y t�@�@@@��@@���,brick_height��b t��c t�@�@@@��@@���'spacing��l t��m t�@�@@@��@@���%level��v t��w t� @�@@@��@���������@��� u#+�� u#,@@@��@��� u#.�� u#/@@@@��� u#*�� u#0@��@@@�����@��� u#3�� u#4@@@����%y_max��� u#6�� u#;@�@@@@��� u#2�� u#<@��@@@@��� u#)�� u#=@��@@@�����&Config-screen_bounds��� u#@�� u#T@�@@@@��� u#%@@��@�����*create_row��� vX^�� vXh@�@@@��@@���!y��� vXi�� vXj@�@@@��@@���'row_idx��� vXk�� vXr@�@@@��A�����+create_cols��� wu��� wu�@�@@@��@@���!x��� wu��� wu�@�@@@��@@���'col_idx��� wu��� wu�@�@@@��@@���#acc��  wu�� wu�@�@@@��������">=�� x��� x��@�@@@��@����'col_idx�� x��� x��@�@@@��@����$cols��# x���$ x��@�@@@@�@@@����#acc��- x���. x��@�@@@���@�����*brick_type��: z���; z��@�@@@��@�����!r��F {���G {��@�@@@�������&Random%float��S {���T {�@�@@@��@���#1.0@��] {��^ {�@@@@�@@@@��a {��@@��������!<��l |
�m |
@�@@@��@����!r��w |
�x |
@�@@@��@���#0.1@��� |
�� |
@@@@�@@@�����%Brick%Bonus��� }%1�� }%<@����������&Random#int��� ~?S�� ~?]@�@@@��@���!1@��� ~?^�� ~?_@@@@�@@@�����!2@��� ����� ���@@@@�����%Brick+SpeedUpBall��� ����� ���@@�@@@�����!3@��� ����� ���@@@@�����%Brick,SlowDownBall��� ����� ��
@@�@@@�����!4@��� ��� �@@@@�����%Brick)ExtraLife��� � �� �/@@�@@@�����!5@��� �0@�� �0A@@@@�����%Brick+RestoreLife��� �0E�� �0V@@�@@@���@��� ����� ���@@@@�����%Brick*ScoreBonus�� ���� ���@��������&Random#int�� ���� ���@�@@@��@���#100@�� ���� ���@@@@��  ����! ���@��@@@�@@@@��& }%=�' ���@���* ~?M
@@@��@@@���������˰�6 �?S�7 �?T@�@@@��@����!r��A �?Q�B �?R@�@@@��@���#0.5@��K �?U�L �?X@@@@�@@@����*Reinforced��T �^j�U �^t@�������!+��_ �^��` �^�@�@@@��@�������&Random#int��n �^v�o �^�@�@@@��@���!2@��x �^��y �^�@@@@�@@@��@���!2@��� �^��� �^�@@@@��� �^u�� �^�@��@@@�6@@@�����'Classic��� ����� ���@@�@@@��� �?N@@@��� |
@@@�8@@@@��� z��	@@��@�����)new_brick��� ����� ���@�@@@�������%Brick&create��� ����� ���@�@@@��@�������!x��� ����� ���@�@@@���������� ����� ���@�@@@��@���������� ����� ���@�@@@��@����%y_max��� ����� ���@�@@@��@����!y��� ����� ���@�@@@@�@@@��@���"0.@��� ����� ���@@@@�@@@@��� ����� ���@��@@@@��@����+brick_width�� ��� ��@�@@@��@����,brick_height�� �� �*@�@@@��@������*brick_type�� �+<�  �+F@�@@@�������%Brick*Reinforced��, �LX�- �Lh@����!n��4 �Li�5 �Lj@�@@@�@@@@����!n��> �Ln�? �Lo@�@@@���@��E ����F ���@@@@���!1@��L ����M ���@@@@��O �+5�P ���@���S �+6@@@��@���������] ����^ ���@�@@@��@�����&Config*base_bonus��j ����k ���@�@@@��@����%level��u ����v ���@�@@@@��y ����z ���@��@@@��@����*brick_type��� ����� ���@�@@@@��@@@@��� ���@@������+create_cols��� ���� ��@�@@@��@���������� ��"�� ��$@�@@@��@������ɰ�� ���� ��@�@@@��@����!x��� ���� ��@�@@@��@����+brick_width��� ���� ��!@�@@@@�@@@��@����'spacing��� ��%�� ��,@�@@@@��� ���� ��-@��@@@��@���������� ��7�� ��8@�@@@��@����'col_idx��� ��/�� ��6@�@@@��@���!1@��� ��9�� ��:@@@@��� ��.�� ��;@��@@@��@����"::��	 ��G�	 ��I@��������)new_brick��	 ��=�	 ��F@�@@@�����#acc��	 ��J�	 ��M@�@@@@�A@@��	 ��<�	 ��N@��@@@@��@@@��@@@��@@@��	% x��@@@�'	A@@�2
A@@�=A@@@��	* wuy@@������+create_cols��	3 �VZ�	4 �Ve@�@@@��@����'spacing��	> �Vf�	? �Vm@�@@@��@���!0@��	H �Vn�	I �Vo@@@��@����"[]��	R �Vp�	S �Vr@@�@@@@�#@@@�-@@@��A@@��A@@@��	Z vXZ@@��A�����+create_rows��	d ����	e ���@�@@@��@@���!y��	n ����	o ���@�@@@��@@���'row_idx��	x ����	y ���@�@@@��@@���#acc��	� ����	� ���@�@@@��������">=��	� ����	� ���@�@@@��@����'row_idx��	� ����	� ���@�@@@��@����$rows��	� ����	� ���@�@@@@�@@@����#acc��	� �� �	� ��@�@@@���@�����*row_bricks��	� ��	� �!@�@@@������*create_row��	� �$�	� �.@�@@@��@����!y��	� �/�	� �0@�@@@��@����'row_idx��	� �1�	� �8@�@@@@�@@@@��	� �@@������+create_rows��	� �<B�	� �<M@�@@@��@��������	� �<a�	� �<c@�@@@��@������!��
 �<Q�
 �<S@�@@@��@����!y��
 �<O�
 �<P@�@@@��@����,brick_height��
 �<T�
 �<`@�@@@@�@@@��@����'spacing��
% �<d�
& �<k@�@@@@��
) �<N�
* �<l@��@@@��@������ذ�
6 �<v�
7 �<w@�@@@��@����'row_idx��
A �<n�
B �<u@�@@@��@���!1@��
K �<x�
L �<y@@@@��
N �<m�
O �<z@��@@@��@������!@��
\ �<��
] �<�@�@@@��@����*row_bricks��
g �<|�
h �<�@�@@@��@����#acc��
r �<��
s �<�@�@@@@��
v �<{�
w �<�@��@@@@��@@@��@@@��
} ���@@@��A@@�	A@@�
A@@@��
� ���@@������+create_rows��
� ����
� ���@�@@@��@����'spacing��
� ����
� ���@�@@@��@���!0@��
� ����
� ���@@@��@����X��
� ����
� ���@@�@@@@�"@@@�,@@@�U@@@��@@@�;A@@�FA@@�Q	A@@�\
A@@�gA@@�rA@@@��
� t��@@�@���@�����&create��
� ����
� ���@�@@@��@@����"()��
� ����
� ���@@�@@@��@�����&layout��
� ����
� ���@�@@@������6calculate_brick_layout��
� ����
� ���@�@@@��@�����&Config+brick_width��
� ����
� ��@�@@@��@�����&Config,brick_height��
� ���  ��@�@@@��@�����&Config-screen_bounds�� ��� ��,@�@@@��@�����&Config-brick_spacing�� ��-� ��A@�@@@@�8@@@@�� ���@@��@�����&bricks��( ����) ���@�@@@������1create_brick_grid��3 ����4 ���@�@@@��@������&layout��@ ����A ���@�@@@��$rows��G ����H ���@�
@@@��@������&layout��T ����U ���@�@@@��$cols��[ ����\ ���@�
@@@��@�����&Config+brick_width��h ����i ��@�@@@��@�����&Config,brick_height��u ���v ��%@�@@@��@�����&Config-brick_spacing��� ��&�� ��:@�@@@��@�����&Config-initial_level��� ��;�� ��O@�@@@@�`@@@@��� ���@@��@�����(quadtree��� �T\�� �Td@�@@@�������(Quadtree.build_quadtree��� �Tg�� �T~@�@@@��@����&bricks��� �T�� �T�@�@@@��@�����&Config-screen_bounds��� �T��� �T�@�@@@@�@@@@��� �TX@@������$ball��� ����� ���@�������$Ball&create��� ����� ���@�@@@��@���������
��� ����� ���@�@@@��@������#fst��� ����� ���@�@@@��@�����&Config/paddle_position�� ���� ���@�@@@@�@@@��@������"/.�� ���� ���@�@@@��@�����&Config,paddle_width��! ����" ���@�@@@��@���"2.@��+ ����, ���@@@@�@@@@�6@@@�������U��7 ��9�8 ��;@�@@@��@������a��C �� �D ��"@�@@@��@������#snd��P ���Q ��@�@@@��@�����&Config/paddle_position��] ��	�^ ��@�@@@@�@@@��@�����&Config-paddle_height��k ��#�l ��7@�@@@@�@@@��@���#10.@��v ��<�w ��?@@@@�)@@@@��z ����{ ��@@���@@@��@���������#~-.��� �AJ�� �AL@�@@@��@�����&Config2initial_ball_speed��� �Ae@�@@@@�@@@������&Config2initial_ball_speed��� �Ag�� �A�@�@@@@��� �AI�� �A�@��!@@@��@�����&Config+ball_radius��� ����� ���@�@@@��@�����&Config)ball_mass��� ����� ���@�@@@@��@@@����&paddle��� ����� ���@�������&Paddle&create��� ����� ���@�@@@��@�����&Config/paddle_position��� ����� ���@�@@@��@�����&Config,paddle_width��� ����� ���@�@@@��@�����&Config-paddle_height��  �� � ��@�@@@��@�����&Config/paddle_velocity�� ��� ��+@�@@@@�8@@@����&bricks�� �-3� �-9@�����@@@����%score��$ �;A�% �;F@�����&Config-initial_score��. �;I�/ �;]@�@@@����%lives��7 �_e�8 �_j@�����&Config-initial_lives��A �_m�B �_�@�@@@����%level��J ����K ���@�����&Config-initial_level��T ����U ���@�@@@����&status��] ����^ ���@����(Starting��e ����f ���@@�@@@����+last_update��n ����o ���@�������$Unix,gettimeofday��z ����{ ���@�@@@��@�������� ����� ���@@�@@@@�@@@����(quadtree��� ����� ���@�����@@@����+bonus_items��� ���� ��@����Q��� ���� ��@@�@@@����.active_effects��� ��� �'@����a��� �*�� �,@@�@@@����3active_ball_effects��� �.4�� �.G@����q��� �.J�� �.L@@�@@@@@��� ����� �uz@@@�@@@�6@@@��@@@��A@@@��� ���@@�@@���J _ r�� �|@@@���J _ _@�@���@�������A@@@�������)unset_lib@@��@���@@@@@@@@@