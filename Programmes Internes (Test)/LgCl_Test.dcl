LONGCUMUL_ResultBox:dialog {
  width = 80 ;
  label = "LONGCUMUL : Affichage des résultats" ;
  :row {
    :column {
      children_fixed_width = true ;
      :boxed_column {
        label = "Calques" ;
        :toggle {
          label = "Considération de la propriété" ;
          key = "Layer_Chk" ;
          value = "1" ;
        }
        :list_box {
          width = 30 ;
          height = 7 ;
          key = "Layer_lst" ;
          multiple_select = true ;
        }
      }
      spacer ;
      :boxed_row {
        label = "Objets" ;
        :list_box {
          width = 30 ;
          height = 7 ;
          key = "Object_lst" ;
          multiple_select = true ;
        }
      }
    }
    :boxed_column {
      label = "Informations" ;
      children_fixed_width = true ;
      :list_box {
        key = "ResultBox" ;
//        is_enabled = false ;
        width = 80 ;
        height = 50 ;
      }
    }
  }
  spacer ;
  ok_cancel ;
}