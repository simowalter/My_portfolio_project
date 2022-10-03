Public Class Form1
    Dim Mario As acteur
    Dim counter_timer_retour_mario As Integer = 0
    Dim counter_timer_avance_mario As Integer = 0
    Dim counter_timer_saute_1_mario As Integer = 0
    Dim counter_timer_saute_2_mario As Integer = 0
    Dim K As Integer = 0
    Dim ok_descend As Boolean = False
    Dim stop_descend As Boolean = False
    Dim chutte_sol_ok As Boolean = False
    Dim ok_avance As Boolean = True
    Dim ok_retour As Boolean = True
    Dim tab_bonus_box(6) As PictureBox
    Dim tab_wall_box(8) As PictureBox
    Dim abcisse_debut_etage_1 As Integer
    Dim abcisse_debut_etage_2 As Integer
    Dim ordonne_rez_chausse As Integer
    Dim ordonne_etage_1 As Integer
    Dim ordonne_etage_2 As Integer
    Dim longueur_etage_1 As Integer
    Dim longueur_etage_2 As Integer
    Dim hauteur_etage As Integer
    Dim tab_mario(5) As Image
    Dim pas_avance As Point
    Dim pas_saut As Point
    Dim a As Integer = 15
    Dim score As Integer = 0
    Dim nb_coins As Integer = 0
    Dim nb_vie As Integer = 3
    Dim coordonne_start_mario As Point
    Dim coordonne_start_ennemis_sol(4) As Point
    Dim coordonne_start_ennemi_air(4) As Point
    Dim coordonne_start_tortue_volante_sol(3) As Point
    Dim coordonne_start_tortue_volante_air(4) As Point
    Dim tab_ennemi_sol(5) As PictureBox
    Dim tab_ennemi_air(4) As PictureBox
    Dim tab_tortue_volante_air(4) As PictureBox
    Dim tab_tortue_volant_sol(3) As PictureBox
    Dim level As Integer = 1
    Dim tab_ennemi_mort(5) As Integer
    Dim temps As Integer = 150000
    Dim numero_brique As Integer
    Dim mario_petit As Boolean = True
    Dim premier_vague_ennemi_mort As Boolean = False
    Dim nb_ennemi_vague1_mort As Integer = 0
    Dim deuxieme_vague_ennemi_mort As Boolean = False
    Dim nb_ennemi_vague2_mort As Integer = 0
    Dim mario_mort As Boolean = False
    Dim mario_en_plein_saut As Boolean = False
    Dim pas_saut_tortue_volante As Point
    Dim etat_ennemi_air(5) As Integer
    Dim etat_tortue_volante_sol(3) As Integer  ' ce tableau servira a sovaoir l'etat de la tortue si les ailes sont deja coupe  ou si elle est deja carapasse
    Dim etat_tortue_volante_air(4) As Integer ' ce tableau servira a sovaoir l'etat de la tortue si les ailes sont deja coupe  ou si elle est deja carapasse
    Dim nb_ennemi_vague3_mort As Integer = 0
    Dim ok_pause = False
    Dim ok_level_3 As Boolean = False
  
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim m As Point
        Dim s As Boolean = True
        abcisse_debut_etage_1 = PictureBox_first_box_step1.Location.X
        abcisse_debut_etage_2 = PictureBox_fist_coin_step2.Location.X
        ordonne_rez_chausse = PictureBox_sol.Location.Y
        ordonne_etage_1 = PictureBox_first_box_step1.Location.Y
        ordonne_etage_2 = PictureBox_fist_coin_step2.Location.Y
        longueur_etage_1 = PictureBox_last_box_step1.Location.X + PictureBox_last_box_step1.Width - PictureBox_first_box_step1.Location.X
        longueur_etage_2 = PictureBox_3_coin_box_step2.Location.X + PictureBox_3_coin_box_step2.Width - PictureBox_fist_coin_step2.Location.X
        hauteur_etage = PictureBox_fist_coin_step2.Height

        tab_ennemi_sol(0) = PictureBox_enemi_sol_1
        tab_ennemi_sol(1) = PictureBox_ennemi_sol_2
        tab_ennemi_sol(2) = PictureBox_ennemi_sol_3
        tab_ennemi_sol(3) = PictureBox_ennemi_sol_4
        tab_ennemi_sol(4) = PictureBox_sol_tortue

        tab_ennemi_air(0) = PictureBox_sol_tortue
        tab_ennemi_air(1) = PictureBox_tortu2_etage1_air
        tab_ennemi_air(2) = PictureBox_tortu3_etage1_air
        tab_ennemi_air(3) = PictureBox_tortu3_etage2_air
        m.X = 60
        m.Y = 0

        tab_tortue_volante_air(0) = PictureBox_tortue_volante_etage1_1
        tab_tortue_volante_air(1) = PictureBox_tortue_volante_etage1_2
        tab_tortue_volante_air(2) = PictureBox_tortue_volante_etage2_1
        tab_tortue_volante_air(3) = PictureBox_tortue_volante_etage2_2

        tab_tortue_volant_sol(0) = PictureBox_tortue_volante1
        tab_tortue_volant_sol(1) = PictureBox_tortue_volante2
        tab_tortue_volant_sol(2) = PictureBox_tortue_volante_3

        coordonne_start_tortue_volante_air(0) = PictureBox_tortue_volante_etage1_1.Location
        coordonne_start_tortue_volante_air(1) = PictureBox_tortue_volante_etage1_2.Location
        coordonne_start_tortue_volante_air(2) = PictureBox_tortue_volante_etage2_1.Location
        coordonne_start_tortue_volante_air(3) = PictureBox_tortue_volante_etage2_2.Location

        coordonne_start_tortue_volante_sol(0) = PictureBox_tortue_volante1.Location
        coordonne_start_tortue_volante_sol(1) = PictureBox_tortue_volante1.Location + m
        coordonne_start_tortue_volante_sol(2) = PictureBox_tortue_volante1.Location + m + m

        coordonne_start_ennemi_air(0) = PictureBox_enemi_sol_1.Location
        coordonne_start_ennemi_air(1) = PictureBox_tortu2_etage1_air.Location
        coordonne_start_ennemi_air(2) = PictureBox_tortu3_etage1_air.Location
        coordonne_start_ennemi_air(3) = PictureBox_sol_tortue.Location

        coordonne_start_mario = PictureBox_mario.Location

        coordonne_start_ennemis_sol(0) = PictureBox_enemi_sol_1.Location
        coordonne_start_ennemis_sol(1) = PictureBox_enemi_sol_1.Location + m
        coordonne_start_ennemis_sol(2) = PictureBox_enemi_sol_1.Location + m + m
        coordonne_start_ennemis_sol(3) = PictureBox_enemi_sol_1.Location + m + m + m

        For i As Integer = 0 To 2
            etat_tortue_volante_sol(i) = -1 ' -1 code que la tortue volante n'a pas encore ete touche
            etat_tortue_volante_air(i) = -1
        Next
        etat_tortue_volante_air(3) = -1
        Mario = New acteur(PictureBox_mario.Location, PictureBox_mario.Size)
        tab_mario(0) = My.Resources.Mario
        tab_mario(1) = My.Resources.Mario___Walk1
        tab_mario(2) = My.Resources.Mario___Walk2
        tab_mario(3) = My.Resources.Mario___Walk3
        tab_mario(4) = My.Resources.Mario___Skid

        tab_bonus_box(0) = PictureBox_first_box_step1
        tab_bonus_box(1) = PictureBox_box_coin_midle_step1
        tab_bonus_box(2) = PictureBox_last_box_step1
        tab_bonus_box(3) = PictureBox_fist_coin_step2
        tab_bonus_box(4) = PictureBox_2_coin_box_step2
        tab_bonus_box(5) = PictureBox_3_coin_box_step2

        tab_wall_box(0) = PictureBox_brique1
        tab_wall_box(1) = PictureBox_brique2
        tab_wall_box(2) = PictureBox_brique3
        tab_wall_box(3) = PictureBox_brique4
        tab_wall_box(4) = PictureBox_brique5
        tab_wall_box(5) = PictureBox_brique6
        tab_wall_box(6) = PictureBox_brique7
        tab_wall_box(7) = PictureBox_brique8

        pas_saut_tortue_volante.X = 3
        pas_saut_tortue_volante.Y = 16
        pas_avance.X = PictureBox_mario.Width / 5
        pas_avance.Y = 0
        pas_saut.X = 0
        pas_saut.Y = PictureBox_mario.Height

        For i As Integer = 0 To 4
            tab_mario(i).RotateFlip(RotateFlipType.Rotate180FlipY)

        Next
        For i As Integer = 0 To 3
            tab_ennemi_mort(i) = 0
        Next
        Timer_ennemi_sol1.Enabled = True
        Timer_time_game.Enabled = True
        Me.Enabled = True
        For i As Integer = 0 To 3
            etat_ennemi_air(i) = -1
        Next

    End Sub
    Function compris_entre(ByVal M As Integer, ByVal a As Integer, ByVal b As Integer)

        If a <= b Then
            If M >= a And M <= b Then
                Return True
            Else
                Return False
            End If
        Else
            If M >= b And M <= a Then
                Return True
            Else
                Return False
            End If
        End If

    End Function
    Sub avance()
        Dim N As Point
        chute_si_vide()
        avance_ok()
        If ok_avance Then
            If PictureBox_mario.Location.X + pas_avance.X < Me.Width Then
                PictureBox_mario.Location = PictureBox_mario.Location + pas_avance
            Else
                N.X = 0
                N.Y = PictureBox_mario.Location.Y
                PictureBox_mario.Location = N
            End If
        End If

    End Sub
    Sub retour()
        Dim M As Point
        chute_si_vide()
        retour_ok()
        If ok_retour Then
            If PictureBox_mario.Location.X - pas_avance.X > 0 Then
                PictureBox_mario.Location = PictureBox_mario.Location - pas_avance
            Else
                M.X = Me.Width
                M.Y = PictureBox_mario.Location.Y
                PictureBox_mario.Location = M
            End If
        End If
    End Sub
    Sub saute_1()
        Dim M As Point
        M.X = 0
        M.Y = (PictureBox_mario.Height) / 2
        If PictureBox_mario.Location.X < PictureBox_first_box_step1.Location.X Or PictureBox_mario.Location.X > (PictureBox_last_box_step1.Location.X + 10) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1

        ElseIf PictureBox_mario.Location.X - PictureBox_first_box_step1.Location.X >= -5 And PictureBox_mario.Location.X - PictureBox_first_box_step1.Location.X <= 10 Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            PictureBox_first_box_step1.Image = My.Resources.Empty_Block
            ok_descend = True
            K = K + 1
            PictureBox_box_on_first_box_coin.Visible = True
            PictureBox_box_on_first_box_coin.Image = My.Resources.coin
            Timer_coin.Enabled = True
        ElseIf PictureBox_mario.Location.X - PictureBox_last_box_step1.Location.X <= -5 And PictureBox_mario.Location.X - PictureBox_last_box_step1.Location.X <= 10 Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            PictureBox_last_box_step1.Image = My.Resources.Empty_Block
            ok_descend = True
            K = K + 1
            PictureBox_box_on_middle_box_coin.Visible = True
            PictureBox_box_on_middle_box_coin.Image = My.Resources.coin
            Timer_coin.Enabled = True
        ElseIf PictureBox_mario.Location.X - PictureBox_box_coin_midle_step1.Location.X <= -5 And PictureBox_mario.Location.X - PictureBox_box_coin_midle_step1.Location.X <= 10 Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            PictureBox_box_coin_midle_step1.Image = My.Resources.Empty_Block
            ok_descend = True
            K = K + 1
            PictureBox_box_on_last_box_coin.Visible = True
            PictureBox_box_on_last_box_coin.Image = My.Resources.coin
            Timer_coin.Enabled = True
        ElseIf PictureBox_mario.Location.Y - M.Y >= PictureBox_first_box_step1.Location.Y Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            ok_descend = True
            K = K + 1
        End If
    End Sub

    Sub saute_2()
        Dim M As Point
        M.X = 0
        M.Y = (PictureBox_mario.Height)
        ' si mario est au rez de chausse et pas en bas du premier etage il peut sauter sans conttainte
        If Not (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_1, abcisse_debut_etage_1 + longueur_etage_1 + PictureBox_last_box_step1.Width)) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1
            ' sinon si mario est en bas du premier box du premier etage il saute et casse celui ci
        Else
            If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_first_box_step1.Location.X, a, PictureBox_first_box_step1.Width + a) And PictureBox_sol.Location.Y - PictureBox_mario.Location.Y = PictureBox_mario.Height Then
                PictureBox_mario.Location = PictureBox_mario.Location - M
                PictureBox_first_box_step1.Image = My.Resources.Empty_Block
                ok_descend = True
                K = K + 1
                score = score + 100
                nb_coins = nb_coins + 1
                Label_coin.Text = nb_coins
                Label_score.Text = score
                PictureBox_box_on_first_box_coin.Visible = True
                PictureBox_box_on_first_box_coin.Image = My.Resources.coin
                Timer_coin.Enabled = True
                ' sinon si mario est en bas du dernier box du premier etage il saute et casse celui ci
            ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_last_box_step1.Location.X, a, PictureBox_last_box_step1.Width + a) And PictureBox_sol.Location.Y - PictureBox_mario.Location.Y = PictureBox_mario.Height Then
                PictureBox_mario.Location = PictureBox_mario.Location - M
                PictureBox_last_box_step1.Image = My.Resources.Empty_Block
                ok_descend = True
                K = K + 1
                score = score + 100
                nb_coins = nb_coins + 1
                Label_coin.Text = nb_coins
                Label_score.Text = score
                PictureBox_box_on_last_box_coin.Visible = True
                PictureBox_box_on_last_box_coin.Image = My.Resources.coin
                Timer_coin.Enabled = True
                ' sinon si mario est en bas du deuxieme box du premier etage il saute et casse celui ci
            ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_box_coin_midle_step1.Location.X, a, PictureBox_box_coin_midle_step1.Width + a) And PictureBox_sol.Location.Y - PictureBox_mario.Location.Y = PictureBox_mario.Height Then
                PictureBox_mario.Location = PictureBox_mario.Location - M
                PictureBox_box_coin_midle_step1.Image = My.Resources.Empty_Block
                ok_descend = True
                K = K + 1
                score = score + 100
                nb_coins = nb_coins + 1
                Label_coin.Text = nb_coins
                Label_score.Text = score
                PictureBox_box_on_middle_box_coin.Visible = True
                PictureBox_box_on_middle_box_coin.Image = My.Resources.coin
                Timer_coin.Enabled = True
                'sinon si mario est en bas du premier etage il a un saut reduit et retombe aussitot
            Else
                If PictureBox_mario.Location.Y + PictureBox_mario.Height = ordonne_rez_chausse Then
                    PictureBox_mario.Location = PictureBox_mario.Location - M
                    ok_descend = True
                    K = K + 1
                    For i As Integer = 0 To 7
                        If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_wall_box(i).Location.X, a, tab_wall_box(i).Width + a) Then
                            numero_brique = i + 1
                            Timer_brique.Enabled = True
                        End If
                    Next
                End If
            End If
        End If
        If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_first_box_step1.Location.X, -1, a - 1) And compris_entre(PictureBox_mario.Location.Y - PictureBox_first_box_step1.Location.Y, 0, hauteur_etage + 3) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1 ' on verifie si on est tout proche du premier box du premier etage on monte sur le premier etage
            M.X = abcisse_debut_etage_1 - 26
            M.Y = PictureBox_first_box_step1.Location.Y - PictureBox_mario.Height
            PictureBox_mario.Location = M
            stop_descend = True
        ElseIf compris_entre(PictureBox_mario.Location.X - (PictureBox_last_box_step1.Location.X + PictureBox_last_box_step1.Width), -(a - 1), 1) And compris_entre(PictureBox_mario.Location.Y - PictureBox_last_box_step1.Location.Y, 0, hauteur_etage + 3) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1  ' on verifie si on est tout proche du dernier box du premier etage  on monte sur le premier etage
            M.X = abcisse_debut_etage_1 + longueur_etage_1 - 1
            M.Y = PictureBox_last_box_step1.Location.Y - PictureBox_mario.Height
            PictureBox_mario.Location = M
            stop_descend = True
        ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_fist_coin_step2.Location.X, -1, a - 1) And compris_entre(PictureBox_mario.Location.Y - PictureBox_fist_coin_step2.Location.Y, 0, hauteur_etage + 3) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1                ' on verifie si on est tout proche du premier box du second etage et au desssus on monte sur le deuxieme etage
            M.X = abcisse_debut_etage_2 - 26
            M.Y = PictureBox_fist_coin_step2.Location.Y - PictureBox_mario.Height
            PictureBox_mario.Location = M
            stop_descend = True
            counter_timer_saute_2_mario = 10
            Exit Sub
        ElseIf compris_entre(PictureBox_mario.Location.X - (PictureBox_3_coin_box_step2.Location.X + PictureBox_3_coin_box_step2.Width), -(a - 1), 1) And compris_entre(PictureBox_mario.Location.Y - PictureBox_3_coin_box_step2.Location.Y, 0, hauteur_etage + 3) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1               ' on verifie si on est tout proche du dernier box du second etage et au desssus on monte sur le deuxieme etage
            M.X = abcisse_debut_etage_2 + longueur_etage_2 - 1
            M.Y = PictureBox_3_coin_box_step2.Location.Y - PictureBox_mario.Height
            PictureBox_mario.Location = M
            stop_descend = True
        End If
        ' si on se trouve sur le premier etage on peut sauter 
        If PictureBox_mario.Location.Y <> ordonne_etage_2 - PictureBox_mario.Height And PictureBox_mario.Location.Y + PictureBox_mario.Height <= ordonne_etage_1 And (Not (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_2 + a + 1, abcisse_debut_etage_2 + longueur_etage_2 + PictureBox_3_coin_box_step2.Width - 10))) And compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_1 + a, abcisse_debut_etage_1 + longueur_etage_1 + PictureBox_last_box_step1.Width) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1
            ' sinon si on est sur le premier etgae en bas du premier box du deuxieme etage on peut sauter et casser le box
        ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_fist_coin_step2.Location.X, a, PictureBox_fist_coin_step2.Width + a) And PictureBox_mario.Location.Y = ordonne_etage_1 - PictureBox_mario.Height Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            PictureBox_fist_coin_step2.Image = My.Resources.Empty_Block
            ok_descend = True
            K = K + 1
            score = score + 100
            nb_coins = nb_coins + 1
            Label_coin.Text = nb_coins
            Label_score.Text = score
            PictureBox_bonus_on_first_box_step2.Visible = True
            PictureBox_bonus_on_first_box_step2.Image = My.Resources.coin
            Timer_coin.Enabled = True
            'sinon si on est sur le premier etgae en bas du deuxieme box du deuxieme etage on peut sauter et casser le box
        ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_2_coin_box_step2.Location.X, a, PictureBox_2_coin_box_step2.Width + a) And PictureBox_mario.Location.Y = ordonne_etage_1 - PictureBox_mario.Height Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            PictureBox_2_coin_box_step2.Image = My.Resources.Empty_Block
            ok_descend = True
            K = K + 1
            score = score + 100
            nb_coins = nb_coins + 1
            Label_coin.Text = nb_coins
            Label_score.Text = score
            PictureBox_bonus_on_2_box_step2.Visible = True
            PictureBox_bonus_on_2_box_step2.Image = My.Resources.coin
            Timer_coin.Enabled = True
            'sinon si on est sur le premier etgae en bas du dernier box du deuxieme etage on peut sauter et casser le box
        ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - PictureBox_3_coin_box_step2.Location.X, a, PictureBox_3_coin_box_step2.Width + a) And PictureBox_mario.Location.Y = ordonne_etage_1 - PictureBox_mario.Height Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            PictureBox_3_coin_box_step2.Image = My.Resources.Empty_Block
            ok_descend = True
            K = K + 1
            score = score + 100
            nb_coins = nb_coins + 1
            Label_coin.Text = nb_coins
            Label_score.Text = score
            PictureBox_bonus_on_3_box_step2.Visible = True
            PictureBox_bonus_on_3_box_step2.Image = My.Resources.coin
            Timer_coin.Enabled = True
        End If
        'si on est sur l'etage 2 on peut sauter
        If PictureBox_mario.Location.Y <= ordonne_etage_2 - PictureBox_mario.Height And compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_2 + a + 1, abcisse_debut_etage_2 + longueur_etage_2 + PictureBox_3_coin_box_step2.Width - a) Then
            PictureBox_mario.Location = PictureBox_mario.Location - M
            K = K + 1
        End If
    End Sub
    Sub abaisse_mario()
        Dim m As Point
        m.X = PictureBox_mario.Location.X
        m.Y = PictureBox_mario.Location.Y - PictureBox_mario.Height
        PictureBox_mario.Size = My.Resources.Mario___Duck.Size
        m.Y = m.Y - PictureBox_mario.Height
        PictureBox_mario.Image = My.Resources.Mario___Duck
    End Sub
    Sub debout_mario()
        PictureBox_mario.Size = My.Resources.Mario.Size
        PictureBox_mario.Image = My.Resources.Mario
    End Sub
    Sub descend_1()
        Dim M As Point
        M.Y = PictureBox_mario.Height / 2
        M.X = 0
        PictureBox_mario.Location = PictureBox_mario.Location + M

        ' a corriger vue les conditions du terrain 
    End Sub
    Sub descend_2()
        descend_terre_ok()
        If Not stop_descend Then
            If PictureBox_mario.Location.Y + PictureBox_mario.Height + pas_saut.Y <= ordonne_rez_chausse Then
                PictureBox_mario.Location = PictureBox_mario.Location + pas_saut
            End If
        End If
    End Sub
    Sub descend()
        Dim M As Point
        M.Y = 2
        M.X = 0
        descend_terre_ok()
        If Not stop_descend Then
            If PictureBox_mario.Location.Y + PictureBox_mario.Height + M.Y <= PictureBox_sol.Location.Y Then
                PictureBox_mario.Location = PictureBox_mario.Location + M
            End If
        End If
    End Sub


    Sub Form1_KeyDown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles Me.KeyDown

        Select Case e.KeyCode
            Case Keys.Right
                avance_mario()
            Case Keys.Left
                retour_mario()
            Case Keys.Up
                stop_descend = False
                saute_2_mario()
            Case Keys.Down
                chute_si_vide()
                descend_terre_ok()
                If Not stop_descend Then
                    descend()
                    debout_mario()
                Else
                    abaisse_mario()
                End If
            Case Keys.RShiftKey
                stop_descend = False
                saute_1_mario()
            Case Keys.D And Keys.E
                Timer_cour_avance_mario.Enabled = True
            Case Keys.A And Keys.E
                Timer_cour_recule_mario.Enabled = True
            Case Keys.Escape
                End
            Case Keys.Space
                Timer_avance_mario.Enabled = False
                Timer_retour_mario.Enabled = False
                Timer_saute_1_mario.Enabled = False
                Timer_saute_2_mario.Enabled = False
                Timer_cour_avance_mario.Enabled = False
                Timer_cour_recule_mario.Enabled = False
                PictureBox_mario.Image = My.Resources.Mario
        End Select

    End Sub
    Private Sub form1_keyup(ByVal sender As System.Object, ByVal e As KeyEventArgs) Handles Me.KeyUp
        Mario = New acteur(PictureBox_mario.Location, PictureBox_mario.Size)
        Select Case e.KeyCode
            Case Keys.Right Or Keys.D
                PictureBox_mario.Image = My.Resources.Mario
                Timer_avance_mario.Enabled = False
                Timer_cour_avance_mario.Enabled = False
            Case Keys.Left Or Keys.A
                PictureBox_mario.Image = My.Resources.Mario
                Timer_retour_mario.Enabled = False
                Timer_cour_recule_mario.Enabled = False
        End Select
    End Sub
    Sub avance_mario()
        Timer_avance_mario.Enabled = True
    End Sub
    Sub retour_mario()
        Timer_retour_mario.Enabled = True
    End Sub

    Private Sub Timer_mvt_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_avance_mario.Tick
        If ok_avance Then
            counter_timer_avance_mario = counter_timer_avance_mario + 1
            Select Case counter_timer_avance_mario Mod 4
                Case 1
                    PictureBox_mario.Image = My.Resources.Mario___Walk1
                    avance()
                Case 2
                    PictureBox_mario.Image = My.Resources.Mario___Walk2
                    avance()
                Case 3
                    PictureBox_mario.Image = My.Resources.Mario___Walk3
                    avance()
                Case 0
                    PictureBox_mario.Image = My.Resources.Mario
                    Mario = New acteur(PictureBox_mario.Location, PictureBox_mario.Size)
                    Mario.avance()
                    PictureBox_mario.Location = Mario.emplacement
                    Timer_avance_mario.Enabled = False
                    counter_timer_avance_mario = 0
            End Select
        Else
            PictureBox_mario.Image = My.Resources.Mario

        End If

    End Sub

    Private Sub Timer_retour_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_retour_mario.Tick
        chute_si_vide()
        ok_retour = True
        retour_ok()
        If ok_retour Then
            counter_timer_retour_mario = counter_timer_retour_mario + 1

            Select Case counter_timer_retour_mario Mod 4
                Case 1
                    PictureBox_mario.Image = tab_mario(1)
                    retour()

                Case 2
                    PictureBox_mario.Image = tab_mario(2)
                    retour()


                Case 3
                    PictureBox_mario.Image = tab_mario(3)
                    retour()

                Case 0
                    PictureBox_mario.Image = tab_mario(0)
                    Mario = New acteur(PictureBox_mario.Location, PictureBox_mario.Size) ' ON UTILISE LA CLASSE ACTEUR (UNE POSSIBILITE)
                    Mario.retour()
                    PictureBox_mario.Location = Mario.emplacement
                    Timer_retour_mario.Enabled = False
                    counter_timer_avance_mario = 0
            End Select
        Else
            PictureBox_mario.Image = My.Resources.Mario

        End If

    End Sub
    Sub saute_1_mario()
        Timer_saute_1_mario.Enabled = True
    End Sub
    Sub saute_2_mario()
        Timer_saute_2_mario.Enabled = True
    End Sub
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        avance_mario()
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        retour_mario()
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        stop_descend = False
        saute_1_mario()
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        stop_descend = False
        saute_2_mario()
    End Sub

    Private Sub ExitToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        End
    End Sub

    Private Sub Timer_saute_1_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_saute_1_mario.Tick
        counter_timer_saute_1_mario = counter_timer_saute_1_mario + 1
        PictureBox_mario.Image = My.Resources.Mario___Jump
        Select Case counter_timer_saute_1_mario Mod 4
            Case 1
                saute_2()
            Case 2
                If Not ok_descend Then
                    saute_2()
                End If
                descend_terre_ok()
            Case 3
                If K >= 1 And Not stop_descend Then
                    descend_2()
                End If
                descend_terre_ok()
            Case 0
                If K >= 2 And Not stop_descend Then
                    descend_2()
                    descend_terre_ok()
                End If
                PictureBox_mario.Image = My.Resources.Mario
                counter_timer_saute_1_mario = 0
                Timer_saute_1_mario.Enabled = False
                K = 0
                stop_descend = False
                ok_descend = False
        End Select
    End Sub

    Private Sub Timer_saute_2_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_saute_2_mario.Tick
        counter_timer_saute_2_mario = counter_timer_saute_2_mario + 1
        PictureBox_mario.Image = My.Resources.Mario___Jump
        mario_en_plein_saut = True
        Select Case counter_timer_saute_2_mario Mod 8
            Case 1
                K = 0
                saute_2()
                descend_terre_ok()
            Case 2
                If Not ok_descend Then
                    saute_2()
                End If
                descend_terre_ok()

            Case 3
                If Not ok_descend Then
                    saute_2()
                End If
                descend_terre_ok()
            Case 4
                If Not ok_descend Then
                    saute_2()
                End If
                descend_terre_ok()
            Case 5
                If K >= 1 And Not stop_descend Then
                    descend_2()
                End If
                descend_terre_ok()
            Case 6
                If K >= 2 And Not stop_descend Then
                    descend_2()
                End If
                descend_terre_ok()
            Case 7
                If K >= 3 And Not stop_descend Then
                    descend_2()
                End If
                descend_terre_ok()
            Case 0
                If K >= 4 And Not stop_descend Then
                    descend_2()
                    descend_terre_ok()
                End If
                PictureBox_mario.Image = My.Resources.Mario
                counter_timer_saute_2_mario = 0
                Timer_saute_2_mario.Enabled = False
                K = 0
                ok_descend = False
                stop_descend = False
                mario_en_plein_saut = False
        End Select

    End Sub

    Private Sub PlaySoundToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PlaySoundToolStripMenuItem.Click
        If OpenFileDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            My.Computer.Audio.Play(OpenFileDialog1.FileName, AudioPlayMode.Background)
        End If
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        chute_si_vide()
        descend_terre_ok()
        If Not stop_descend Then
            descend()
            debout_mario()
        Else
            abaisse_mario()
        End If
    End Sub
    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Timer_avance_mario.Enabled = False
        Timer_retour_mario.Enabled = False
        Timer_saute_1_mario.Enabled = False
        Timer_saute_2_mario.Enabled = False
        Timer_cour_avance_mario.Enabled = False
        Timer_cour_recule_mario.Enabled = False
        PictureBox_mario.Image = My.Resources.Mario
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        PictureBox_mario.Image = My.Resources.Mario___Skid
        Timer_cour_avance_mario.Enabled = True
    End Sub

    Private Sub Timer_cour_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_cour_avance_mario.Tick
        chute_si_vide()
        avance_ok()
        If ok_avance Then
            PictureBox_mario.Image = My.Resources.Mario___Skid
            avance()
        Else
            PictureBox_mario.Image = My.Resources.Mario
            Timer_cour_avance_mario.Enabled = False
            Timer_chute_vide.Enabled = True
        End If

    End Sub

    Private Sub Timer_cour_recule_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_cour_recule_mario.Tick
        chute_si_vide()
        ok_retour = True
        retour_ok()
        If ok_retour Then
            retour()
            PictureBox_mario.Image = tab_mario(4)
        Else
            PictureBox_mario.Image = tab_mario(0)
            Timer_cour_recule_mario.Enabled = False
            Timer_chute_vide.Enabled = True
        End If

    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        Dim image As Image = My.Resources.Mario___Skid
        image.RotateFlip(RotateFlipType.Rotate180FlipY)
        PictureBox_mario.Image = image
        Timer_cour_recule_mario.Enabled = True
    End Sub

    Private Sub PlayALiToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PlayALiToolStripMenuItem.Click
        If Me.FolderBrowserDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            For Each song As String In IO.Directory.GetFiles(FolderBrowserDialog1.SelectedPath)
                My.Computer.Audio.Play(song, AudioPlayMode.Background)
            Next
        End If
    End Sub


    Private Sub Timer_coin_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_coin.Tick
        PictureBox_box_on_first_box_coin.Image = Nothing
        PictureBox_box_on_last_box_coin.Image = Nothing
        PictureBox_box_on_middle_box_coin.Image = Nothing
        PictureBox_bonus_on_2_box_step2.Image = Nothing
        PictureBox_bonus_on_3_box_step2.Image = Nothing
        PictureBox_bonus_on_first_box_step2.Image = Nothing
        PictureBox_box_on_first_box_coin.Visible = False
        PictureBox_box_on_last_box_coin.Visible = False
        PictureBox_box_on_middle_box_coin.Visible = False
        PictureBox_bonus_on_2_box_step2.Visible = False
        PictureBox_bonus_on_3_box_step2.Visible = False
        PictureBox_bonus_on_first_box_step2.Visible = False

        Timer_coin.Enabled = False
    End Sub
    ''' <summary>
    ''' ' cette fonction permet de stabiliser mario si on est sur un etage 
    ''' </summary>
    ''' <remarks></remarks>
    Sub descend_terre_ok()
        Dim M As Point
        ' si l'abcisse de mario est entre les abcisses de l'etage 1
        If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_1, abcisse_debut_etage_1 + longueur_etage_1 + PictureBox_last_box_step1.Width - 5) Then
            ' si mario est tres pres et en haut de l'etage 1
            If compris_entre(ordonne_etage_1 - (PictureBox_mario.Location.Y + PictureBox_mario.Height), 0, 5) Then
                ' on le met sur l'etage 1
                M.Y = ordonne_etage_1 - PictureBox_mario.Height
                M.X = PictureBox_mario.Location.X
                PictureBox_mario.Location = M
                stop_descend = True
                Timer_chute_vide.Enabled = False
            End If
            ' si l'abcisse de  mario est entre les abcisses de l'etage 2
        ElseIf compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_2, abcisse_debut_etage_2 + longueur_etage_2 + PictureBox_3_coin_box_step2.Width - 5) Then
            ' si mario est tres pres et en haut de l'etage 2
            If compris_entre(ordonne_etage_2 - (PictureBox_mario.Location.Y + PictureBox_mario.Height), 0, 5) Then
                ' on le met sur l'etage 2
                M.Y = ordonne_etage_2 - PictureBox_mario.Height
                M.X = PictureBox_mario.Location.X
                PictureBox_mario.Location = M
                stop_descend = True
                Timer_chute_vide.Enabled = False
            End If

            ' si mario est tres proche du rez de chausse 
        ElseIf compris_entre(ordonne_rez_chausse - (PictureBox_mario.Location.Y + PictureBox_mario.Height), 0, 5) Then
            ' on le met au rez de chaussee
            M.Y = PictureBox_sol.Location.Y - PictureBox_mario.Height
            M.X = PictureBox_mario.Location.X
            PictureBox_mario.Location = M
            stop_descend = True
            Timer_chute_vide.Enabled = False
        Else
            Timer_chute_vide.Enabled = True
        End If

    End Sub
    Sub chute_si_vide()
        Dim M As Point
        chutte_sol_ok = False
        ' si l'abcisse de mario n'est pas entre ceux de l'etage 1 et mario est a la hauteur de l'etage1 
        If (Not (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_1, abcisse_debut_etage_1 + longueur_etage_1 + PictureBox_last_box_step1.Width))) And (PictureBox_mario.Location.Y = ordonne_etage_1 - PictureBox_mario.Height And Timer_saute_2_mario.Enabled = False) Then
            ' on active le timer pour la chutte de mario
            Timer_chute_vide.Enabled = True
            If compris_entre(ordonne_rez_chausse - (PictureBox_mario.Location.Y + PictureBox_mario.Height), 0, 5) Then
                'si mario est tres proche du rez de chausse on le pose sur le rez de chausse 
                M.X = PictureBox_mario.Location.X
                M.Y = ordonne_rez_chausse - PictureBox_mario.Height
                PictureBox_mario.Location = M
                chutte_sol_ok = True
            End If
        End If
        ' si l'abcisse de mario n'est pas entre ceux de l'etage 2 et mario est a la hauteur de l'etage2 
        If (Not (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width, abcisse_debut_etage_2, abcisse_debut_etage_2 + longueur_etage_2 + PictureBox_3_coin_box_step2.Width))) And (PictureBox_mario.Location.Y = ordonne_etage_2 - PictureBox_mario.Height And Timer_saute_2_mario.Enabled = False) Then
            ' on active le timer pour la chutte de mario 
            Timer_chute_vide.Enabled = True

            If compris_entre(ordonne_etage_1 - (PictureBox_mario.Location.Y + PictureBox_mario.Height), 0, 5) Then
                'si mario est tres proche de l'etage 1 on le pose sur celui ci 
                M.X = PictureBox_mario.Location.X
                M.Y = ordonne_etage_1 - PictureBox_mario.Height
                PictureBox_mario.Location = M
                chutte_sol_ok = True
            End If

        End If
    End Sub

    Private Sub Timer_chute_vide_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_chute_vide.Tick
        'si la chute n'est pas termine et mario n'est pas en train de sauter

        If Not chutte_sol_ok Then
            'on descend 
            descend()
            chute_si_vide()
            'sinon
        Else
            'on desactive la chutte de mario
            Timer_chute_vide.Enabled = False
        End If
    End Sub
    Sub avance_ok()
        ' si mario est assez proche et derriere le premier box de l'etage 1 
        If compris_entre(PictureBox_first_box_step1.Location.X - (Me.PictureBox_mario.Location.X + PictureBox_mario.Width), 0, 1) And compris_entre(PictureBox_mario.Location.Y, ordonne_etage_1, ordonne_etage_1 + PictureBox_first_box_step1.Height) Then
            ' on desactive l'avancement 
            ok_avance = False
            ' si mario est assez proche et derriere le premier box de l'etage 2
        ElseIf compris_entre(PictureBox_fist_coin_step2.Location.X - (Me.PictureBox_mario.Location.X + PictureBox_mario.Width), 0, 1) And compris_entre(PictureBox_mario.Location.Y, ordonne_etage_2 + PictureBox_fist_coin_step2.Height, ordonne_etage_2) Then
            ' on desactive l'avancement
            ok_avance = False
        Else
            'sinon on peut avancer
            ok_avance = True
        End If
    End Sub
    Sub retour_ok()
        ' si mario est assez proche et devant  le dernier box de l'etage 1 
        If compris_entre(Me.PictureBox_mario.Location.X - (abcisse_debut_etage_1 + longueur_etage_1), 0, 1) And compris_entre(PictureBox_mario.Location.Y, ordonne_etage_1, ordonne_etage_1 + PictureBox_last_box_step1.Height) Then
            ' on desactive le retour 
            ok_retour = False
            ' si mario est assez proche et devant  le dernier box de l'etage 2
        ElseIf compris_entre(Me.PictureBox_mario.Location.X - (abcisse_debut_etage_2 + longueur_etage_2), 0, 1) And compris_entre(PictureBox_mario.Location.Y, ordonne_etage_2, ordonne_etage_2 + PictureBox_3_coin_box_step2.Height) Then
            ' on desactive le retour 
            ok_retour = False
        Else
            'sinon on peut avancer
            ok_retour = True
        End If
    End Sub

    Private Sub Timer_ennemi_sol1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_ennemi_sol1.Tick
        Dim M As Point
        M.X = 3
        M.Y = 0
       'si l'ennemi touche mario on active le timer de mort de mario
        For i As Integer = 0 To 3
            If (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_ennemi_sol(i).Location.X, 1, -1) Or compris_entre(PictureBox_mario.Location.X - (tab_ennemi_sol(i).Location.X + tab_ennemi_sol(i).Width), -1, 1)) And PictureBox_mario.Location.Y = tab_ennemi_sol(i).Location.Y Then
                For l As Integer = 0 To 100000000
                    ' ceci servira a marquer une legere pause avant l'envolee du corps de mario 
                Next
                timer_chute_mort_mario.Enabled = True
                mario_mort = True
                mario_mort = False
                Exit Sub
            End If
        Next
        'on avance chaque ennemi
        For i As Integer = 0 To 3
            'si un ennemi est deja applatit alors il disparait et se met hors du plateau de jeu
            If tab_ennemi_mort(i) = 1 Then
                tab_ennemi_sol(i).Visible = False
                M.X = Me.Width + 200
                M.Y = ordonne_rez_chausse + 400
                tab_ennemi_sol(i).Location = M
            Else
                'si l'ennemi n'a pas atteint l'extremite gauche il avance 
                If tab_ennemi_sol(i).Location.X > 0 And tab_ennemi_sol(i).Visible = True Then
                    tab_ennemi_sol(i).Location = tab_ennemi_sol(i).Location - M
                    'sinon il 
                Else
                    If tab_ennemi_sol(i).Location.X <= 0 And tab_ennemi_sol(i).Visible = True Then
                        M.X = Me.Width
                        M.Y = ordonne_rez_chausse - tab_ennemi_sol(0).Height
                        tab_ennemi_sol(i).Location = M   ' here is a pb 
                    End If
                End If
                ' si mario est au dessus d'un ennemi il l'applatit 
                If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_ennemi_sol(i).Location.X, 1, tab_ennemi_sol(i).Width + PictureBox_mario.Width - 1) And (compris_entre(PictureBox_mario.Location.Y + PictureBox_mario.Height - tab_ennemi_sol(i).Location.Y, 0, 32)) And mario_mort = False Then
                    M.X = tab_ennemi_sol(i).Location.X
                    M.Y = ordonne_rez_chausse - My.Resources.Goomba___Grey___Stomp.Height
                    tab_ennemi_sol(i).Image = My.Resources.Goomba___Grey___Stomp
                    tab_ennemi_sol(i).Location = M
                    nb_ennemi_vague1_mort = nb_ennemi_vague1_mort + 1
                    tab_ennemi_mort(i) = 1 ' c'est a dire que cet ennemi est mort
                    score = score + 150
                    Label_score.Text = score
                    Beep()
                End If
            End If
        Next
    End Sub

    Private Sub timer_chute_mort_mario_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles timer_chute_mort_mario.Tick
        Static k As Integer = 0 ' k servira de compteur pour faire sauter mario quand il est mort 
        Dim m As Point
        m.X = 0
        m.Y = PictureBox_mario.Height
        'si on est a la premiere vague d'ennemi pendant la mort de mario les ennemi sont immobiles
        If nb_ennemi_vague1_mort < 4 Then
            Timer_ennemi_sol1.Enabled = False
        End If
        'mario saute 5 fois 
        PictureBox_mario.Image = My.Resources.Mario___Dead
        If compris_entre(k, 0, 4) Then
            PictureBox_mario.Location = PictureBox_mario.Location - m
            ' et descend 16 fois
        ElseIf compris_entre(k, 5, 20) Then
            PictureBox_mario.Location = PictureBox_mario.Location + m
        End If
        If k = 20 Then
            k = 0
            PictureBox_mort_mario.SizeMode = PictureBoxSizeMode.CenterImage
            PictureBox_mort_mario.Image = My.Resources.Mario___Dead
            PictureBox_mort_mario.BringToFront()
            PictureBox_mort_mario.Visible = True
            Timer_pause.Enabled = True
            For l As Integer = 0 To 1000000
                'pause
            Next
            timer_chute_mort_mario.Enabled = False
            Me.Enabled = True
            If nb_vie = 0 Then
                TextBox_gain_ou_perte.Text = "YOU ARE A LOSER!!!"
                TextBox_gain_ou_perte.Visible = True
                PictureBox_mort_mario.BringToFront()
                PictureBox_mort_mario.Visible = True
                If MsgBox("voulez vous recommencer?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                    TextBox_gain_ou_perte.Visible = False
                    Me.Enabled = True
                    reinitialiser_plateau()
                    timer_chute_mort_mario.Enabled = False
                ElseIf MsgBox("voulez vous quitter l'application?", MsgBoxStyle.OkCancel) = MsgBoxResult.Ok Then
                    End
                End If
            ElseIf nb_vie >= 1 Then
                nb_vie = nb_vie - 1
                Label_live.Text = nb_vie
                PictureBox_mario.Location = coordonne_start_mario
                PictureBox_mario.Image = My.Resources.Mario
            Else
            End If
        End If
        k = k + 1
        Timer_ennemi_sol1.Enabled = True
        ok_pause = False
    End Sub

    Sub reinitialiser_plateau()

        Me.Enabled = True
        PictureBox_mario.Location = coordonne_start_mario
        PictureBox_mario.Image = My.Resources.Mario
        TextBox_gain_ou_perte.Visible = False
        For i As Integer = 0 To 5
            tab_bonus_box(i).Image = My.Resources.Question_Block
        Next
        For i As Integer = 0 To 3
            tab_ennemi_sol(i).Image = My.Resources.Goomba
            tab_ennemi_sol(i).Location = coordonne_start_ennemis_sol(i)
            tab_ennemi_sol(i).Visible = True
            tab_ennemi_air(i).Location = coordonne_start_ennemi_air(i)
            tab_ennemi_air(i).Visible = False
            tab_tortue_volante_air(i).Visible = False
            tab_tortue_volante_air(i).Location = coordonne_start_tortue_volante_air(i)
        Next
        For i As Integer = 0 To 2
            tab_tortue_volant_sol(i).Visible = False
            tab_tortue_volant_sol(i).Location = coordonne_start_tortue_volante_sol(i)
        Next

        nb_ennemi_vague1_mort = 0
        nb_ennemi_vague2_mort = 0
        score = 0
        nb_coins = 0
        nb_vie = 3
        level = 1
        temps = 150000
        Label_coin.Text = nb_coins
        Label_live.Text = nb_vie
        Label_score.Text = score
        Label_level.Text = level
        Label_time.Text = temps
    End Sub

    Private Sub Timer_time_game_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_time_game.Tick
        If nb_ennemi_vague1_mort = 4 And Timer_chutte_ennemi_vague_3.Enabled = False Then
            Me.BackColor = Color.Aquamarine
            Timer_chutte_ennemi_air.Enabled = True
            For i As Integer = 0 To 3
                tab_ennemi_air(i).Visible = True
            Next
        ElseIf nb_ennemi_vague2_mort = 4 And Timer_chutte_ennemi_vague_3.Enabled = False Then
            For i As Integer = 0 To 2
                tab_tortue_volante_air(i).Visible = True
                tab_tortue_volant_sol(i).Visible = True
            Next
            tab_tortue_volante_air(3).Visible = True
            Me.BackColor = Color.Beige
            Timer_chutte_ennemi_vague_3.Enabled = True
            For i As Integer = 0 To 2
                tab_tortue_volant_sol(i).Visible = True
                tab_tortue_volante_air(i).Visible = True
            Next
            tab_tortue_volante_air(3).Visible = True
        End If
        If nb_ennemi_vague1_mort = 4 Then
            Timer_chutte_ennemi_air.Enabled = True
        End If
        If nb_ennemi_vague2_mort = 4 And Timer_chutte_ennemi_vague_3.Enabled = False Then
            Timer_pause.Enabled = True
            If ok_level_3 = True Then
                Timer_chutte_ennemi_air.Enabled = False
                For i As Integer = 0 To 2
                    tab_tortue_volante_air(i).Visible = True
                    tab_tortue_volant_sol(i).Visible = True
                Next
                tab_tortue_volante_air(3).Visible = True
                If Timer_chutte_ennemi_vague_3.Enabled = False Then
                    Me.BackColor = Color.BlueViolet
                    Timer_chutte_ennemi_vague_3.Enabled = True
                    Timer_vol_tortu.Enabled = False
                End If
            End If
        End If
        If Timer_chutte_ennemi_air.Enabled = True Then
            level = 2
        ElseIf Timer_chutte_ennemi_vague_3.Enabled = True Then
            level = 3
        ElseIf Timer_ennemi_sol1.Enabled = True Then
            level = 1
        End If
        If nb_ennemi_vague3_mort = 4 Then
            Timer_chutte_ennemi_vague_3.Enabled = False
            If MsgBox("quitter le jeu?", MsgBoxStyle.OkCancel) = MsgBoxResult.Ok Then
                End
            End If
        End If
        temps = temps - 1000
        Label_time.Text = temps / 1000
    End Sub

    Private Sub Timer_brique_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_brique.Tick
        Static k As Integer = 0
        Dim M As Point
        k = k + 1
        M.X = 0
        M.Y = tab_wall_box(0).Height / 3
        ' si mario est petit
        If mario_petit = True Then
            ' si le numero de brique est correct 
            If numero_brique <> 0 Then
                If k = 1 Then
                    tab_wall_box(numero_brique - 1).Location = tab_wall_box(numero_brique - 1).Location - M
                ElseIf k = 2 Then
                    tab_wall_box(numero_brique - 1).Location = tab_wall_box(numero_brique - 1).Location + M
                    k = 0
                    Timer_brique.Enabled = False
                    numero_brique = 0
                End If
            Else
                Timer_brique.Enabled = False
            End If
            ' sinon si mario est grand 
        Else
            tab_wall_box(numero_brique - 1).Visible = False

        End If
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_chutte_ennemi_air.Tick
        Dim m As Point
        m.X = 1
        m.Y = 0
        'si l'ennemi est sur le plateau et est encore tortue et est avant l'extremite il avance
        If etat_ennemi_air(0) = -1 Then
            If tab_ennemi_air(0).Location.X > 0 Then
                tab_ennemi_air(0).Location = tab_ennemi_air(0).Location - m
            Else
                'sinon il se met a l'autre extremite
                m.X = Me.Width
                m.Y = tab_ennemi_air(0).Location.Y
                tab_ennemi_air(0).Location = m
            End If
        End If
        If etat_ennemi_air(1) = -1 Then
            'tant que l'ennemi n'a pas atteint l'etage 1 il descend
            If tab_ennemi_air(1).Location.Y + tab_ennemi_air(1).Height < ordonne_etage_1 Then
                m.Y = 5
                m.X = 0
                tab_ennemi_air(1).Location = tab_ennemi_air(1).Location + m
                'si il est tres proche de l'etage 1 on le met sur celui ci
                If compris_entre(tab_ennemi_air(1).Location.Y + tab_ennemi_air(1).Height - ordonne_etage_1, -10, 0) Then
                    m.X = tab_ennemi_air(1).Location.X
                    m.Y = ordonne_etage_1 - tab_ennemi_air(1).Height
                    tab_ennemi_air(1).Location = m
                End If
            End If
            ' si l'ennemi est sur l'etage 1
            If tab_ennemi_air(1).Location.Y + tab_ennemi_air(1).Height = ordonne_etage_1 Then
                'si l'ennemi n'a pas atteint le debut de l'etage 1 il avance
                If tab_ennemi_air(1).Location.X + tab_ennemi_air(1).Width > abcisse_debut_etage_1 Then
                    m.X = 1
                    m.Y = 0
                    tab_ennemi_air(1).Location = tab_ennemi_air(1).Location - m
                Else
                    ' sinon l'ennemi descend 
                    m.X = 0
                    m.Y = 30
                    tab_ennemi_air(1).Location = tab_ennemi_air(1).Location + m
                End If
            End If
            'si l'ennemi est entre le rez de chausse et l'etage 1 il se met au rez de chausse
            If compris_entre(tab_ennemi_air(1).Location.Y + tab_ennemi_air(1).Height, ordonne_rez_chausse, ordonne_etage_1 + a) Then
                m.X = tab_ennemi_air(1).Location.X
                m.Y = ordonne_rez_chausse - tab_ennemi_air(1).Height
                tab_ennemi_air(1).Location = m
            End If
            'si l'ennemi est sur le rez de chausse 
            If tab_ennemi_air(1).Location.Y + tab_ennemi_air(1).Height = ordonne_rez_chausse Then
                ' si l'ennemi n'a pas atteint l'extremite gauche il avance
                If tab_ennemi_air(1).Location.X > 0 Then
                    m.X = 1
                    m.Y = 0
                    tab_ennemi_air(1).Location = tab_ennemi_air(1).Location - m
                    'sinon il ressort a l'autre extremite 
                Else
                    m.X = Me.Width
                    m.Y = tab_ennemi_air(1).Location.Y
                    tab_ennemi_air(1).Location = m
                End If
            End If
        End If
       
        'ceci bas est pour le deuxieme ennemi 
        'tant que l'ennemi n'a pas atteint l'etage 1 il descend
        If etat_ennemi_air(2) = -1 Then
            If tab_ennemi_air(2).Location.Y + tab_ennemi_air(2).Height < ordonne_etage_1 Then
                m.Y = 5
                m.X = 0
                tab_ennemi_air(2).Location = tab_ennemi_air(2).Location + m
                'si il est tres proche de l'etage 1 on le met sur celui ci
                If compris_entre(tab_ennemi_air(2).Location.Y + tab_ennemi_air(2).Height - ordonne_etage_1, -10, 0) Then
                    m.X = tab_ennemi_air(2).Location.X
                    m.Y = ordonne_etage_1 - tab_ennemi_air(2).Height
                    tab_ennemi_air(2).Location = m
                End If
            End If
            ' si l'ennemi est sur l'etage 1
            If tab_ennemi_air(2).Location.Y + tab_ennemi_air(2).Height = ordonne_etage_1 Then
                'si l'ennemi n'a pas atteint le debut de l'etage 1 il avance
                If tab_ennemi_air(2).Location.X + tab_ennemi_air(2).Width > abcisse_debut_etage_1 Then
                    m.X = 1
                    m.Y = 0
                    tab_ennemi_air(2).Location = tab_ennemi_air(2).Location - m
                Else
                    ' sinon l'ennemi descend 
                    m.X = 0
                    m.Y = 30
                    tab_ennemi_air(2).Location = tab_ennemi_air(2).Location + m
                End If
            End If
            'si l'ennemi est entre le rez de chausse et l'etage 1 il se met au rez de chausse
            If compris_entre(tab_ennemi_air(2).Location.Y + tab_ennemi_air(2).Height, ordonne_rez_chausse, ordonne_etage_1 + a) Then
                m.X = tab_ennemi_air(2).Location.X
                m.Y = ordonne_rez_chausse - tab_ennemi_air(2).Height
                tab_ennemi_air(2).Location = m
            End If
            'si l'ennemi est sur le rez de chausse 
            If tab_ennemi_air(2).Location.Y + tab_ennemi_air(2).Height = ordonne_rez_chausse Then
                ' si l'ennemi n'a pas atteint l'extremite gauche il avance
                If tab_ennemi_air(2).Location.X > 0 Then
                    m.X = 1
                    m.Y = 0
                    tab_ennemi_air(2).Location = tab_ennemi_air(2).Location - m
                    'sinon il ressort a l'autre extremite 
                Else
                    m.X = Me.Width
                    m.Y = tab_ennemi_air(2).Location.Y
                    tab_ennemi_air(2).Location = m
                End If
            End If
        End If

        For i As Integer = 0 To 3
            'si mario est au dessus de l'ennemi 
            If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_ennemi_air(i).Location.X, 0, tab_ennemi_air(i).Width - 1) And (compris_entre(PictureBox_mario.Location.Y + PictureBox_mario.Height - tab_ennemi_air(i).Location.Y, 0, 32)) Then
                If etat_ennemi_air(i) = -1 Then
                    m.X = tab_ennemi_air(i).Location.X
                    m.Y = ordonne_rez_chausse - My.Resources.Green_Koopa_Troopa___Shell1.Height
                    tab_ennemi_air(i).Location = m
                    tab_ennemi_air(i).Image = My.Resources.Green_Koopa_Troopa___Shell1
                    etat_ennemi_air(i) = 0 '0 code la tortue est carapasse
                ElseIf etat_ennemi_air(i) = 0 Then
                    etat_ennemi_air(i) = 1 ' 1 code la tortue est prete a l'envole
                    score = score + 200
                    Label_score.Text = score
                    nb_ennemi_vague2_mort = nb_ennemi_vague2_mort + 1
                    Timer_vol_tortu.Enabled = True
                End If

            End If
        Next
        For i As Integer = 0 To 3
            ' si mario est tres proche l'ennemi  etant sur le meme etage il meurt 
            If (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_ennemi_air(i).Location.X, 1, -1) Or compris_entre(PictureBox_mario.Location.X - (tab_ennemi_sol(i).Location.X + tab_ennemi_air(i).Width), -1, 1)) And PictureBox_mario.Location.Y + PictureBox_mario.Height = tab_ennemi_air(i).Location.Y + tab_ennemi_air(i).Height Then
                If etat_ennemi_air(i) = -1 Then
                    For l As Integer = 0 To 10000000
                        'pause
                    Next
                    timer_chute_mort_mario.Enabled = True
                ElseIf etat_ennemi_air(i) = 0 Then
                    nb_ennemi_vague2_mort = nb_ennemi_vague2_mort + 1
                    etat_ennemi_air(i) = 1 ' 1 code la tortue est prete a l'envole
                    score = score + 200
                    Label_score.Text = score
                    Timer_vol_tortu.Enabled = True
                End If
            End If
        Next
        If etat_ennemi_air(3) = -1 Then
            ' si l'ennemi n'a pas atteint l'etage 2 il descend
            If tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height < ordonne_etage_2 Then
                m.Y = 10
                m.X = 0
                tab_ennemi_air(3).Location = tab_ennemi_air(3).Location + m
                'si l;ennemi est tres proche de l'etage 2 on le met sur l'etage 2
                If compris_entre(tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height - ordonne_etage_2, -11, 0) Then
                    m.X = tab_ennemi_air(3).Location.X
                    m.Y = ordonne_etage_2 - tab_ennemi_air(3).Height
                    tab_ennemi_air(3).Location = m
                End If
            End If
            ' si l'ennemi est sur l'etage 2 et n'a pas atteint l'extremite gauche il avance
            If tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height = ordonne_etage_2 Then
                If tab_ennemi_air(3).Location.X + tab_ennemi_air(3).Width > abcisse_debut_etage_2 Then
                    m.X = 1
                    m.Y = 0
                    tab_ennemi_air(3).Location = tab_ennemi_air(3).Location - m
                End If
            End If
            ' si l'ennemi quitte depasse l'etage 2 il descend por tomber sur l'etage 1
            If tab_ennemi_air(3).Location.X + tab_ennemi_air(3).Width <= abcisse_debut_etage_2 And tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height < ordonne_etage_1 Then
                m.X = 0
                m.Y = 20
                tab_ennemi_air(3).Location = tab_ennemi_air(3).Location + m
                'si l'ennemi est tres proche de l'etage 1 on le met sur celui ci 
                If compris_entre(tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height - ordonne_etage_1, -30, 0) Then
                    m.X = tab_ennemi_air(3).Location.X
                    m.Y = ordonne_etage_1 - tab_ennemi_air(3).Height
                    tab_ennemi_air(3).Location = m
                End If
            End If
            ' si l;ennemi est sur l'etage 1 et n'a pas atteint l'extremite gauche il avance
            If tab_ennemi_air(3).Location.X + tab_ennemi_air(3).Width > abcisse_debut_etage_1 And tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height = ordonne_etage_1 Then
                m.X = 1
                m.Y = 0
                tab_ennemi_air(3).Location = tab_ennemi_air(3).Location - m
            End If
            ' si l'ennemi quitte depasse l'etage 1 il descend por tomber sur le rez de chausse
            If tab_ennemi_air(3).Location.X + tab_ennemi_air(3).Width <= abcisse_debut_etage_1 And tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height < ordonne_rez_chausse Then
                m.X = 0
                m.Y = 20
                tab_ennemi_air(3).Location = tab_ennemi_air(3).Location + m
                'si l'ennemi est tres proche du rez de chausse on le met sur celui ci 
                If compris_entre(tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height - ordonne_rez_chausse, -30, 0) Then
                    m.X = tab_ennemi_air(3).Location.X
                    m.Y = ordonne_rez_chausse - tab_ennemi_air(3).Height
                    tab_ennemi_air(3).Location = m
                End If
            End If
            'Si l'ennemi est sur le rez de chausse 
            If tab_ennemi_air(3).Location.Y + tab_ennemi_air(3).Height = ordonne_rez_chausse Then
                ' si l'ennemi n'a pas atteint l'extremite gauche il avance
                If tab_ennemi_air(3).Location.X > 0 Then
                    m.X = 1
                    m.Y = 0
                    tab_ennemi_air(3).Location = tab_ennemi_air(3).Location - m
                    'sinon il ressort a l'autre extremite 
                Else
                    m.X = Me.Width
                    m.Y = tab_ennemi_air(3).Location.Y
                    tab_ennemi_air(3).Location = m
                End If
            End If
           
        End If
    End Sub


    Private Sub Timer_mort_tortue_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_mort_tortue.Tick
        '  Static k As Integer = 0
        ' k = k + 1
        ' If k Mod 2 = 1 Then
        'tab_ennemi_air(numero_tortu_demi_mort).Image = My.Resources.Green_Koopa_Troopa___Shell2
        '  Else
        '   tab_ennemi_air(numero_tortu_demi_mort).Image = My.Resources.Green_Koopa_Troopa
        '   Timer_mort_tortue.Enabled = False
        '   numero_tortu_demi_mort = -1
        '   End If
    End Sub

    Private Sub Timer_vol_tortu_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_vol_tortu.Tick
        Dim m As Point
        m.X = 10
        m.Y = 0
        ' si on est au niveau 2
        If level = 2 Then
            For k As Integer = 0 To 3
                If etat_ennemi_air(k) = 1 Then
                    If tab_ennemi_air(k).Location.X < Me.Width Then
                        tab_ennemi_air(k).Location = tab_ennemi_air(k).Location + m
                        For i As Integer = 0 To 3
                            ' si i est different de l'indice de la tortue morte 
                            If i <> k Then
                                ' si la carapasse de la tortue morte est assez proche d'une autre tortue  elle glisse et disparait 
                                If compris_entre(tab_ennemi_air(k).Location.X + tab_ennemi_air(k).Width - tab_ennemi_air(i).Location.X, -10, 10) And tab_ennemi_air(k).Location.Y + tab_ennemi_air(k).Height = tab_ennemi_air(i).Location.Y + tab_ennemi_air(i).Height Then
                                    score = score + 200
                                    Label_score.Text = score
                                    nb_ennemi_vague2_mort = nb_ennemi_vague2_mort + 1
                                    m.X = 0
                                    m.Y = 35
                                    tab_ennemi_air(i).Image = My.Resources.Green_Koopa_Troopa___Shell1 ' la tortue cogne devient carapasse
                                    tab_ennemi_air(i).Location = tab_ennemi_air(i).Location - m  ' la carapasse de tortue cogne s'eleve un peu avant de disparaitre
                                    tab_ennemi_air(i).Visible = False
                                    m.X = Me.Width + 50
                                    m.Y = 2 * ordonne_rez_chausse
                                    tab_ennemi_air(i).Location = m ' on place le cadavre de tortu assez loin du tableau de jeu pour qu'il n'ai plus d'interaction avec le tableau
                                    tab_ennemi_air(i).Enabled = False
                                End If
                            End If
                        Next
                    Else
                        tab_ennemi_air(k).Visible = False
                        tab_ennemi_air(k).Enabled = False
                        m.X = Me.Width + 50
                        m.Y = 2 * ordonne_rez_chausse
                        tab_ennemi_air(k).Location = m
                       
                    End If
                End If
            Next
           
        End If

        If level = 3 Then
            m.X = 10
            m.Y = 0
            If Timer_chutte_ennemi_vague_3.Enabled = True Then
                For i As Integer = 0 To 2
                    If etat_tortue_volante_sol(i) = 2 Then
                        If tab_tortue_volant_sol(i).Location.X < Me.Width Then
                            tab_tortue_volant_sol(i).Location = tab_tortue_volant_sol(i).Location + m
                            For j As Integer = 0 To 2
                                If j <> i Then
                                    ' si la carapasse de la tortue morte est assez proche d'une autre tortue  elle glisse et disparait 
                                    If compris_entre(tab_tortue_volant_sol(i).Location.X + tab_tortue_volant_sol(i).Width - tab_tortue_volant_sol(j).Location.X, -10, 10) And tab_tortue_volant_sol(i).Location.Y + tab_tortue_volant_sol(i).Height = tab_tortue_volant_sol(j).Location.Y + tab_tortue_volant_sol(j).Height Then
                                        score = score + 200
                                        nb_ennemi_vague3_mort = nb_ennemi_vague3_mort + 1
                                        Label_score.Text = score
                                        m.X = 0
                                        m.Y = 35
                                        tab_tortue_volant_sol(j).Image = My.Resources.Green_Koopa_Troopa___Shell1 ' la tortue cogne devient carapasse
                                        tab_tortue_volant_sol(j).Location = tab_tortue_volant_sol(i).Location - m  ' la carapasse de tortue cogne s'eleve un peu avant de disparaitre
                                        tab_tortue_volant_sol(j).Visible = False

                                        m.X = Me.Width + 50
                                        m.Y = 2 * ordonne_rez_chausse
                                        tab_tortue_volant_sol(j).Location = m ' on place le cadavre de tortu assez loin du tableau de jeu pour qu'il n'ai plus d'interaction avec le tableau
                                        tab_tortue_volant_sol(j).Enabled = False
                                    End If
                                End If
                            Next
                        End If
                        'sinon si la carapasse est hors du plateau de jeu
                    Else
                        tab_tortue_volant_sol(i).Visible = False
                        m.X = Me.Width + 50
                        m.Y = 2 * ordonne_rez_chausse
                        tab_tortue_volant_sol(i).Location = m
                        tab_tortue_volant_sol(i).Enabled = False

                    End If
                Next
                m.X = 10
                m.Y = 0
                For i As Integer = 0 To 3
                    If etat_tortue_volante_air(i) = 2 Then
                        If tab_tortue_volante_air(i).Location.X < Me.Width Then
                            tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location + m
                            For j As Integer = 0 To 3
                                If j <> i Then
                                    ' si la carapasse de la tortue morte est assez proche d'une autre tortue  elle glisse et disparait 
                                    If compris_entre(tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width - tab_tortue_volante_air(j).Location.X, -10, 10) And tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = tab_tortue_volante_air(j).Location.Y + tab_tortue_volante_air(j).Height Then
                                        score = score + 200
                                        nb_ennemi_vague3_mort = nb_ennemi_vague3_mort + 1
                                        Label_score.Text = score
                                        m.X = 0
                                        m.Y = 35
                                        tab_tortue_volante_air(j).Image = My.Resources.Green_Koopa_Troopa___Shell1 ' la tortue cogne devient carapasse
                                        tab_tortue_volante_air(j).Location = tab_tortue_volante_air(i).Location - m  ' la carapasse de tortue cogne s'eleve un peu avant de disparaitre
                                        tab_tortue_volante_air(j).Visible = False
                                        m.X = Me.Width + 50
                                        m.Y = 2 * ordonne_rez_chausse
                                        tab_tortue_volante_air(j).Location = m ' on place le cadavre de tortu assez loin du tableau de jeu pour qu'il n'ai plus d'interaction avec le tableau
                                        tab_tortue_volante_air(j).Enabled = False
                                    End If
                                End If
                            Next
                        End If
                        'sinon si la carapasse est hors du plateau de jeu
                    Else
                        tab_tortue_volante_air(i).Visible = False
                        m.X = Me.Width + 50
                        m.Y = 2 * ordonne_rez_chausse
                        tab_tortue_volante_air(i).Location = m
                        tab_tortue_volante_air(i).Enabled = False

                    End If
                Next
                If nb_ennemi_vague2_mort = 4 Then
                    Timer_chutte_ennemi_vague_3.Enabled = True
                End If
            End If
        End If


    End Sub

   
    Private Sub Timer_chutte_ennemi_vague_3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_chutte_ennemi_vague_3.Tick
        Dim m As Point
        Static k As Integer = 0
        Static arrive_etage_2_2 As Boolean = False
        Static arrive_etage_1_1 As Boolean = False
        Static arrive_etage_1_2() As Boolean = {False, False}
        Static arrive_rez_de_chausse_1() As Boolean = {False, False}
        Static arrive_rez_de_chausse_2() As Boolean = {False, False}
        Static k10 As Integer = 0
        Static k11 As Integer = 0
        Static k20 As Integer = 0
        Static k21 As Integer = 0
        Static k4 As Integer = 0
        Static k5 As Integer = 0
        'si(l) 'ennemi est sur le plateau avant l'extremite il avance
        k = k + 1
        For i As Integer = 0 To 2
            If tab_tortue_volant_sol(i).Location.X > 0 Then
                If etat_tortue_volante_sol(i) = -1 Then
                    m.X = pas_saut_tortue_volante.X
                    If k Mod 2 = 1 Then
                        m.Y = 16
                    Else
                        m.Y = -16
                    End If
                    'si la tortue a perdu les ailes
                ElseIf etat_tortue_volante_sol(i) = 0 Then
                    ' elle ne peut plus voler
                    m.Y = 0
                    'si la tortue est carapasse
                ElseIf etat_tortue_volante_sol(i) = 1 Then
                    m.X = 0
                    m.Y = 0
                End If
                tab_tortue_volant_sol(i).Location = tab_tortue_volant_sol(i).Location - m
            Else
                'sinon il se met a l'autre extremite
                m.X = Me.Width
                m.Y = tab_tortue_volant_sol(i).Location.Y
                tab_tortue_volant_sol(i).Location = m
            End If
        Next
        For i As Integer = 0 To 1
            'tant que l'ennemi n'a pas atteint l'etage 1 il descend
            If tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height < ordonne_etage_1 - pas_saut_tortue_volante.Y - 1 Then
                m.Y = 30
                m.X = 0
                tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location + m
                'si il est tres proche de l'etage 1 on le met sur celui ci 
                If compris_entre(tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height - ordonne_etage_1 - pas_saut_tortue_volante.Y, -30, 0) And arrive_etage_1_1 = False Then
                    m.X = tab_tortue_volante_air(0).Location.X
                    m.Y = ordonne_etage_1 - tab_tortue_volante_air(0).Height
                    tab_tortue_volante_air(0).Location = m
                    m.X = tab_tortue_volante_air(1).Location.X
                    m.Y = ordonne_etage_1 - tab_tortue_volante_air(1).Height
                    tab_tortue_volante_air(1).Location = m
                    arrive_etage_1_1 = True

                End If
            End If
            ' si l'ennemi est sur l'etage 1 et si l'ennemi n'a pas atteint le debut de l'etage 1 il avance
            If arrive_etage_1_1 = True And (tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_etage_1 Or tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_etage_1 - pas_saut_tortue_volante.Y) And tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width >= abcisse_debut_etage_1 Then
                If i = 0 Then
                    k10 = k10 + 1
                    m.X = pas_saut_tortue_volante.X
                    If etat_tortue_volante_air(i) = -1 Then
                        If k10 Mod 2 = 1 Then
                            m.Y = pas_saut_tortue_volante.Y

                        Else
                            m.Y = -pas_saut_tortue_volante.Y
                        End If
                        'si la tortue a perdu les ailes
                    ElseIf etat_tortue_volante_air(i) = 0 Then
                        ' elle ne peut plus voler
                        m.Y = 0
                    ElseIf etat_tortue_volante_air(i) = 1 Then
                        m.X = 0
                        m.Y = 0
                    End If
                    tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                ElseIf i = 1 Then
                    k11 = k11 + 1
                    m.X = pas_saut_tortue_volante.X
                    If etat_tortue_volante_air(i) = -1 Then
                        If k11 Mod 2 = 1 Then
                            m.Y = pas_saut_tortue_volante.Y
                        Else
                            m.Y = -pas_saut_tortue_volante.Y
                        End If
                        'si la tortue a perdu les ailes
                    ElseIf etat_tortue_volante_air(i) = 0 Then
                        ' elle ne peut plus voler
                        m.Y = 0
                    ElseIf etat_tortue_volante_air(i) = 1 Then
                        m.X = 0
                        m.Y = 0
                    End If
                    tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                End If

            ElseIf tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width < abcisse_debut_etage_1 And arrive_rez_de_chausse_1(i) = False Then
                ' sinon l'ennemi descend 
                m.X = 0
                m.Y = 30
                tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location + m
                'si l'ennemi est proche du rez de chausse 
            End If
            If compris_entre(tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height - ordonne_rez_chausse - pas_saut_tortue_volante.Y - 1, -25, 0) And arrive_rez_de_chausse_1(i) = False Then
                m.X = tab_tortue_volante_air(i).Location.X
                m.Y = ordonne_rez_chausse - tab_tortue_volante_air(i).Height
                tab_tortue_volante_air(i).Location = m
                arrive_rez_de_chausse_1(i) = True
                If i = 0 Then
                    k10 = 0
                ElseIf i = 1 Then
                    k11 = 0
                End If
            End If
            'si l'ennemi est sur le rez de chausse 
            If (tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_rez_chausse Or tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_rez_chausse - pas_saut_tortue_volante.Y) And arrive_rez_de_chausse_1(i) = True Then
                ' si l'ennemi n'a pas atteint l'extremite gauche il avance
                If tab_tortue_volante_air(i).Location.X > 0 Then
                    If i = 0 Then
                        k10 = k10 + 1
                        m.X = pas_saut_tortue_volante.X
                        If etat_tortue_volante_air(i) = -1 Then
                            If k10 Mod 2 = 1 Then
                                m.Y = pas_saut_tortue_volante.Y

                            Else
                                m.Y = -pas_saut_tortue_volante.Y
                            End If
                            'si la tortue a perdu les ailes
                        ElseIf etat_tortue_volante_air(i) = 0 Then
                            ' elle ne peut plus voler
                            m.Y = 0
                        ElseIf etat_tortue_volante_air(i) = 1 Then
                            m.X = 0
                            m.Y = 0
                        End If
                        tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                    ElseIf i = 1 Then
                        k11 = k11 + 1
                        m.X = pas_saut_tortue_volante.X
                        If etat_tortue_volante_air(i) = -1 Then
                            If k11 Mod 2 = 1 Then
                                m.Y = pas_saut_tortue_volante.Y
                            Else
                                m.Y = -pas_saut_tortue_volante.Y
                            End If
                            'si la tortue a perdu les ailes
                        ElseIf etat_tortue_volante_air(i) = 0 Then
                            ' elle ne peut plus voler
                            m.Y = 0
                        ElseIf etat_tortue_volante_air(i) = 1 Then
                            m.X = 0
                            m.Y = 0
                        End If
                        tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                    End If
                Else
                    m.X = Me.Width
                    m.Y = tab_tortue_volante_air(i).Location.Y
                    tab_tortue_volante_air(i).Location = m
                End If
            End If
        Next
        For i As Integer = 2 To 3

            ' si l'ennemi n'a pas atteint l'etage 2 il descend
            If tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height < ordonne_etage_2 - pas_saut_tortue_volante.Y - 1 Then
                m.Y = 30
                m.X = 0
                tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location + m
                'si l;ennemi est tres proche de l'etage 2 on le met sur l'etage 2
                If compris_entre(tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height - ordonne_etage_2 - pas_saut_tortue_volante.Y, -30, 0) And arrive_etage_2_2 = False Then
                    m.X = tab_tortue_volante_air(2).Location.X
                    m.Y = ordonne_etage_2 - tab_tortue_volante_air(2).Height
                    tab_tortue_volante_air(2).Location = m
                    m.X = tab_tortue_volante_air(3).Location.X
                    m.Y = ordonne_etage_2 - tab_tortue_volante_air(3).Height
                    tab_tortue_volante_air(3).Location = m
                    arrive_etage_2_2 = True
                    MsgBox("popo")
                End If
            End If
            ' si l'ennemi est sur l'etage 2 
            If arrive_etage_2_2 = True And (tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_etage_2 Or tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_etage_2 - pas_saut_tortue_volante.Y) And tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width >= abcisse_debut_etage_2 Then
                If tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width > abcisse_debut_etage_2 Then
                    If i = 2 Then
                        k20 = k20 + 1
                        m.X = pas_saut_tortue_volante.X
                        If etat_tortue_volante_air(i) = -1 Then
                            If k20 Mod 2 = 1 Then
                                m.Y = pas_saut_tortue_volante.Y

                            Else
                                m.Y = -pas_saut_tortue_volante.Y
                            End If

                            'si la tortue a perdu les ailes
                        ElseIf etat_tortue_volante_air(i) = 0 Then
                            ' elle ne peut plus voler
                            m.Y = 0
                        ElseIf etat_tortue_volante_air(i) = 1 Then
                            m.X = 0
                            m.Y = 0
                        End If
                        tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                    ElseIf i = 3 Then
                        k21 = k21 + 1
                        m.X = pas_saut_tortue_volante.X
                        If etat_tortue_volante_air(i) = -1 Then
                            If k20 Mod 2 = 1 Then
                                m.Y = pas_saut_tortue_volante.Y
                            Else
                                m.Y = -pas_saut_tortue_volante.Y
                            End If
                            'si la tortue a perdu les ailes
                        ElseIf etat_tortue_volante_air(i) = 0 Then
                            ' elle ne peut plus voler
                            m.Y = 0
                        ElseIf etat_tortue_volante_air(i) = 1 Then
                            m.X = 0
                            m.Y = 0
                        End If
                        tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                    End If
                End If
            End If
            ' si l'ennemi quitte depasse l'etage 2 il descend por tomber sur l'etage 1
            'tant que l'ennemi n'a pas atteint l'etage 1 il descend
            If tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height < ordonne_etage_1 - pas_saut_tortue_volante.Y - 1 And tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width < abcisse_debut_etage_2 Then
                m.Y = 30
                m.X = 0
                tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location + m
                'si il est tres proche de l'etage 1 on le met sur celui ci 
                If compris_entre(tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height - ordonne_etage_1 - pas_saut_tortue_volante.Y, -30, 0) And arrive_etage_1_2(i - 2) = False Then
                    m.X = tab_tortue_volante_air(i).Location.X
                    m.Y = ordonne_etage_1 - tab_tortue_volante_air(i).Height
                    tab_tortue_volante_air(i).Location = m
                    arrive_etage_1_2(i - 2) = True
                    If i = 2 Then
                        k20 = 0
                    ElseIf i = 3 Then
                        k21 = 0
                    End If
                End If
            End If
            ' si l'ennemi est sur l'etage 1 et si l'ennemi n'a pas atteint le debut de l'etage 1 il avance
            If arrive_etage_1_2(i - 2) = True And (tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_etage_1 Or tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_etage_1 - pas_saut_tortue_volante.Y) And tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width >= abcisse_debut_etage_1 Then
                If i = 2 Then
                    k20 = k20 + 1
                    m.X = pas_saut_tortue_volante.X
                    If etat_tortue_volante_air(i) = -1 Then
                        If k20 Mod 2 = 1 Then
                            m.Y = pas_saut_tortue_volante.Y

                        Else
                            m.Y = -pas_saut_tortue_volante.Y
                        End If
                        'si la tortue a perdu les ailes
                    ElseIf etat_tortue_volante_air(i) = 0 Then
                        ' elle ne peut plus voler
                        m.Y = 0
                    ElseIf etat_tortue_volante_air(i) = 1 Then
                        m.X = 0
                        m.Y = 0
                    End If
                    tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                ElseIf i = 3 Then
                    k21 = k21 + 1
                    m.X = pas_saut_tortue_volante.X
                    If etat_tortue_volante_air(i) = -1 Then
                        If k21 Mod 2 = 1 Then
                            m.Y = pas_saut_tortue_volante.Y
                        Else
                            m.Y = -pas_saut_tortue_volante.Y
                        End If
                        'si la tortue a perdu les ailes
                    ElseIf etat_tortue_volante_air(i) = 0 Then
                        ' elle ne peut plus voler
                        m.Y = 0
                    ElseIf etat_tortue_volante_air(i) = 1 Then
                        m.X = 0
                        m.Y = 0
                    End If
                    tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                End If

            ElseIf tab_tortue_volante_air(i).Location.X + tab_tortue_volante_air(i).Width < abcisse_debut_etage_1 And arrive_rez_de_chausse_2(i - 2) = False Then
                ' sinon l'ennemi descend 
                m.X = 0
                m.Y = 30
                tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location + m
                'si l'ennemi est proche du rez de chausse 
            End If
            If compris_entre(tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height - ordonne_rez_chausse - pas_saut_tortue_volante.Y - 1, -25, 0) And arrive_rez_de_chausse_2(i - 2) = False Then
                m.X = tab_tortue_volante_air(i).Location.X
                m.Y = ordonne_rez_chausse - tab_tortue_volante_air(i).Height
                tab_tortue_volante_air(i).Location = m
                arrive_rez_de_chausse_2(i - 2) = True
                If i = 2 Then
                    k20 = 0
                ElseIf i = 3 Then
                    k21 = 0
                End If
            End If
            'si l'ennemi est sur le rez de chausse 
            If (tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_rez_chausse Or tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height = ordonne_rez_chausse - pas_saut_tortue_volante.Y) And arrive_rez_de_chausse_2(i - 2) = True Then
                ' si l'ennemi n'a pas atteint l'extremite gauche il avance
                If tab_tortue_volante_air(i).Location.X > 0 Then
                    If i = 2 Then
                        k20 = k20 + 1
                        m.X = pas_saut_tortue_volante.X
                        If etat_tortue_volante_air(i) = -1 Then
                            If k20 Mod 2 = 1 Then
                                m.Y = pas_saut_tortue_volante.Y
                            Else
                                m.Y = -pas_saut_tortue_volante.Y
                            End If
                            'si la tortue a perdu les ailes
                        ElseIf etat_tortue_volante_air(i) = 0 Then
                            ' elle ne peut plus voler
                            m.Y = 0
                        ElseIf etat_tortue_volante_air(i) = 1 Then
                            m.X = 0
                            m.Y = 0
                        End If
                        tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                    ElseIf i = 3 Then
                        k21 = k21 + 1
                        m.X = pas_saut_tortue_volante.X
                        If etat_tortue_volante_air(i) = -1 Then
                            If k21 Mod 2 = 1 Then
                                m.Y = pas_saut_tortue_volante.Y
                            Else
                                m.Y = -pas_saut_tortue_volante.Y
                            End If
                            'si la tortue a perdu les ailes
                        ElseIf etat_tortue_volante_air(i) = 0 Then
                            ' elle ne peut plus voler
                            m.Y = 0
                        ElseIf etat_tortue_volante_air(i) = 1 Then
                            m.X = 0
                            m.Y = 0
                        End If
                        tab_tortue_volante_air(i).Location = tab_tortue_volante_air(i).Location - m
                    End If
                End If
            End If
        Next
        ''''
        For i As Integer = 0 To 2
            ' si mario est tres proche l'ennemi  etant sur le meme etage il meurt ou  s'il est pres de la carapasse elle s'envole
            If (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_tortue_volant_sol(i).Location.X, -1, 1) Or compris_entre(PictureBox_mario.Location.X - (tab_ennemi_sol(i).Location.X + tab_tortue_volant_sol(i).Width), -1, 1)) And compris_entre(PictureBox_mario.Location.Y + PictureBox_mario.Height - (tab_tortue_volant_sol(i).Location.Y + tab_tortue_volant_sol(i).Height), 0, -pas_saut_tortue_volante.Y) Then
                'si la tortue n'est pas carapasse
                If etat_tortue_volante_sol(i) <> 1 Then
                    For l As Integer = 0 To 10000000
                        'ceci servira a marquer une legere pause
                    Next
                    timer_chute_mort_mario.Enabled = True
                Else
                    Timer_vol_tortu.Enabled = True
                    '2 code la tortue est prete pour l'envol
                    etat_tortue_volante_sol(i) = 2
                    score = score + 200
                    Label_score.Text = score
                    nb_ennemi_vague3_mort = nb_ennemi_vague3_mort + 1
                End If

            End If
        Next
        For i As Integer = 0 To 3
            ' si mario est tres proche l'ennemi  etant sur le meme etage il meurt 
            If (compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_tortue_volante_air(i).Location.X, -1, 1) Or compris_entre(PictureBox_mario.Location.X - (tab_ennemi_sol(i).Location.X + tab_tortue_volante_air(i).Width), -1, 1)) And compris_entre(PictureBox_mario.Location.Y + PictureBox_mario.Height - (tab_tortue_volante_air(i).Location.Y + tab_tortue_volante_air(i).Height), 0, -pas_saut_tortue_volante.Y) Then
                'si la tortue n'est pas carapasse
                If etat_tortue_volante_air(i) <> 1 Then
                    For l As Integer = 0 To 10000000
                        'ceci servira a marquer une legere pause
                    Next
                    timer_chute_mort_mario.Enabled = True
                Else
                    Timer_vol_tortu.Enabled = True
                    etat_tortue_volante_air(i) = 2
                    score = score + 200
                    Label_score.Text = score
                    nb_ennemi_vague3_mort = nb_ennemi_vague3_mort + 1
                End If
            End If
        Next
        For i As Integer = 0 To 3
            'si mario est au dessus de l'ennemi 
            If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_tortue_volante_air(i).Location.X, -2, tab_tortue_volante_air(i).Width - 1) And (compris_entre(PictureBox_mario.Location.Y + PictureBox_mario.Height - tab_tortue_volante_air(i).Location.Y, -5, -32)) Then
                If etat_tortue_volante_air(i) = -1 Then
                    m.X = tab_tortue_volante_air(i).Location.X
                    m.Y = ordonne_rez_chausse - My.Resources.Green_Koopa_Troopa.Height
                    tab_tortue_volante_air(i).Location = m
                    tab_tortue_volante_air(i).Image = My.Resources.Green_Koopa_Troopa
                    etat_tortue_volante_air(i) = 0
                ElseIf etat_tortue_volante_air(i) = 0 Then
                    m.X = tab_tortue_volante_air(i).Location.X
                    m.Y = ordonne_rez_chausse - My.Resources.Green_Koopa_Troopa___Shell1.Height
                    tab_tortue_volante_air(i).Location = m
                    tab_tortue_volante_air(i).Image = My.Resources.Green_Koopa_Troopa___Shell1
                    etat_tortue_volante_air(i) = 1
                ElseIf etat_tortue_volante_air(i) = 1 Then
                    Timer_vol_tortu.Enabled = True
                    score = score + 200
                    Label_score.Text = score
                    nb_ennemi_vague3_mort = nb_ennemi_vague3_mort + 1
                    '2 code la tortue est prete pour l'envol
                    etat_tortue_volante_air(i) = 2
                End If
            End If
        Next
        For i As Integer = 0 To 2
            'si mario est au dessus de l'ennemi 
            If compris_entre(PictureBox_mario.Location.X + PictureBox_mario.Width - tab_tortue_volant_sol(i).Location.X, -2, tab_tortue_volant_sol(i).Width - 1) And (compris_entre(PictureBox_mario.Location.Y + PictureBox_mario.Height - tab_tortue_volant_sol(i).Location.Y, -5, -32)) Then
                'si la tortue n'est pas encore touche alors on coupe ses ailes
                If etat_tortue_volante_sol(i) = -1 Then
                    m.X = tab_tortue_volant_sol(i).Location.X
                    m.Y = ordonne_rez_chausse - My.Resources.Green_Koopa_Troopa.Height
                    tab_tortue_volant_sol(i).Location = m
                    tab_tortue_volant_sol(i).Image = My.Resources.Green_Koopa_Troopa
                    etat_tortue_volante_air(i) = 0
                    'sinon si la tortue n'a plus les ailes on la transforme en carapasse
                ElseIf etat_tortue_volante_sol(i) = 0 Then
                    m.X = tab_tortue_volant_sol(i).Location.X
                    m.Y = ordonne_rez_chausse - My.Resources.Green_Koopa_Troopa___Shell1.Height
                    tab_tortue_volant_sol(i).Location = m
                    tab_tortue_volante_air(i).Image = My.Resources.Green_Koopa_Troopa___Shell1
                    etat_tortue_volante_air(i) = 1
                    ' sinon si la tortue est carapasse elle s'envole
                ElseIf etat_tortue_volante_sol(i) = 1 Then
                    Timer_vol_tortu.Enabled = True
                    score = score + 200
                    Label_score.Text = score
                    nb_ennemi_vague3_mort = nb_ennemi_vague3_mort + 1
                    '2 code la tortue est prete pour l'envol
                    etat_tortue_volante_sol(i) = 2
                End If
            End If
        Next

    End Sub

    Private Sub NouvellePartieToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NouvellePartieToolStripMenuItem.Click
        reinitialiser_plateau()
        PictureBox_mort_mario.Visible = False
    End Sub

    Private Sub DddToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DddToolStripMenuItem.Click
        MsgBox("nb = " & nb_ennemi_vague2_mort)
    End Sub
    Private Sub ClavierToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClavierToolStripMenuItem.Click
        GroupBox1.Enabled = False
        ClavierToolStripMenuItem.Checked = True
        BoutonToolStripMenuItem.Checked = False
    End Sub

    Private Sub BoutonToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BoutonToolStripMenuItem.Click
        GroupBox1.Enabled = True
        ClavierToolStripMenuItem.Checked = False
        BoutonToolStripMenuItem.Checked = True
    End Sub

    Private Sub Timer_pause_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_pause.Tick
        Static k As Integer = 0
        If PictureBox_mort_mario.Visible = False Then
            k = k + 1
            If k = 5 Then
                ok_level_3 = True
                Timer_pause.Enabled = True
            End If
        End If
        PictureBox_mort_mario.Visible = False
        Timer_pause.Enabled = False
    End Sub

   
    Private Sub DhfjfkToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DhfjfkToolStripMenuItem.Click
        If Timer_chutte_ennemi_vague_3.Enabled = True Then
            MsgBox("lojbgj")
        End If
    End Sub
End Class

