chiadat :dialog {label = "Chia dien tich Polyline";
spacer;
:text {key= "tong";alignment= centered;}
spacer;
:boxed_row {label= "Chon 1 trong 2 cach chia";
    :column{children_fixed_width = true;
        :radio_button {label="Theo dien tich               "; key = "theodt"; value = 1;}
        :radio_button {label="Theo ty le                      "; key = "theotl";}
    }
    :column{
        :edit_box {key = "dientich"; width = 20; fixed_width= true;}
        :edit_box {key = "tyle"; width = 20; fixed_width= true;}
    }
}
spacer; spacer;
:popup_list {label= "Do chinh xac"; key= "chinhxac"; width = 40; fixed_width= true;alignment= right;}
spacer; spacer;
:radio_row {label = "Phuong thuc di chuyen duong chia";
    :radio_button {label = "Song song"; key = "song"; value = 1;}
    :radio_button {label = "Quay quanh 1 diem"; key = "quay";}
    spacer;
}
spacer; spacer;
:toggle {label = "Ghi dien tich sau khi chia"; key= "ghi";}

spacer; spacer;
:text{ label= "CHU Y"; alignment= centered;}
:text{ label= "Bam OK, duong chia se di chuyen. Hay cho mot chut..."; alignment= centered;}
spacer; spacer;
ok_cancel;
spacer; spacer;
:text{ label= "Copyright by ssg - www.cadviet.com - February, 2009"; alignment= centered;}
}
