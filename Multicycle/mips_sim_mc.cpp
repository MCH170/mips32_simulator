/*
TYPES ARE BASED ON COMMAND FORMAT (REQUIRE DIFFERENT DECODING)

R-TYPE: ADD, ADDU, AND, NOR, OR, SLT, SLTU, SUB, SUBU

I-TYPE: ADDI, ADDIU, ANDI, ORI, SLTI, ALTIU, SLL, SRL

SPECIAL-I-TYPE: LW, SW, LUI

B-TYPE: BEQ, BNE

J-TYPE: J, JAL, JR
*/

#include<iostream>
#include <fstream>
#include<string>
#include <sstream>
#include <iomanip>
#include<cmath>
#include<bitset>
#include <ctime>
#include <stdlib.h>


#define mmp 256      //max memory positions
#define lbl_max 512  //max label positions

using namespace std;

void print_monitors(bool endd);
int lbl_trans(string label);
void ALU();
void reg_file();

string mipsRegisters[33] = {
        "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
        "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
        "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
        "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra", "-"};

int reg[33];

long memAddress[mmp];
long MEM[mmp];

string label_nm[lbl_max];
int LBL[lbl_max];
int label_count=0;

int PC=0;
bool EOF_flag=0;

int cyc_to_print[100];
int cyc_count=0;

clock_t start_time, end_time;
double userTime;

int mainline, globaltemp=0, FSM=1;
bool uns;

//control signals
bool regdst, jump, branch, memread, memtoreg, memwrite, regwrite, zero, pc_cond, pc_write, iord, ALUsrcA, irwrite;
int aluop, pc_source, ALUsrcB;

//monitors
int /*PC, rs, rt*/ wr, write_data, readA, readB, aluout, target, mem_in, mem_out, prevA, prevB, prevALUout, glinA, glinB;
string instr, opcode, type/*R I J SPI B*/;

string file_name;
int rs, rt, rd, address, immed, unsigned_immed, cycles=1;

int hex_dec(string hexString) { //FROM CHATGPT - HEX TO DECIMAL
    int decimalValue;
    istringstream(hexString) >> hex >> decimalValue;
    return decimalValue;
}

string intToHex(int value) {    //FROM CHATGPT - INTEGER TO HEX
    stringstream stream;

    // Ensure the value fits within 32 bits (4 bytes)
    value &= 0xFFFFFFFF;

    // Convert to hexadecimal
    stream << hex << uppercase << setw(8) << setfill('0') << value;

    return stream.str();
}

string intToBinary(unsigned int num) {  //FROM CHATGPT - INTEGER TO BINARY
    // Ensure the input number is within the range [0, 15]
    num = num & 0xF;

    // Convert the integer to its binary representation
    bitset<4> binary(num);

    // Convert the binary representation to a string
    return binary.to_string();
}

string trim(string in){ //REMOVE 0 INFRONT OF NUMBERS
    int l=in.length();
    bool found=0;
    string out="";

    for(int i=0; i<l; i++){
        if(in[i]=='0' && !found){
            continue;
        }else{
            if(in[i]!='0' || found){
                found=1;
                out+=in[i];
            }
        }
    }

    if(out=="")
        out="0";

    return out;
}

int stringToInt(const string& str) {
    try {
        return stoi(str);
    } catch (const invalid_argument& e) {
        cerr << "Invalid argument: " << e.what() << endl;
        return 0;
    } catch (const out_of_range& e) {
        cerr << "Out of range: " << e.what() << endl;
        return 0;
    }
}

unsigned int directToInt(string hex) {  //HEX TO INTEGER BUT UNSIGNED
    unsigned int sum=0;

    string temp="";

    if(1){
        for(int i=0; i<(8-hex.length()); i++){
            temp+='0';
        }
        for(int i=0; i<hex.length(); i++){
            temp+=hex[i];
        }
    }

    for(int i=0; i<8; i++){
        switch(temp[i]){
            case 'F':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case 'E':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case 'D':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case 'C':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
            break;

            case 'B':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case 'A':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case '9':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '8':
                sum+=pow(2, (((8-i)*4)-1));
            break;

            case '7':
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '6':
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case '5':
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '4':
                sum+=pow(2, (((8-i)*4)-2));
            break;

            case '3':
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '2':
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case '1':
                sum+=pow(2, (((8-i)*4)-4));
            break;
        }
    }

    return sum;
}

string parser(int line){    //RETURNS THE N LINE FROM THE FILE
    ifstream fl;
    string com="";

    EOF_flag=false;
    fl.open(file_name);
	if (!fl.is_open()) {
		exit(-1);
	}
	//FILE OK

    int j=1;
	while (j<=line) {
        if (!getline(fl, com)) {
            EOF_flag=true;
            break; // Reached the end of the file
        }
        j++;
    }

    fl.close();
    return com;
}

string formater(string inp){ //REMOVES SPACES, TABS, NEW LINES
	int len=inp.length();
	string out="";

	for(int i=0; i<len; i++){
		if(inp[i]=='#'){
			break;
		}else{
			if(inp[i]!=' ' && inp[i]!='\t' && inp[i]!='\n'){
				out+=inp[i];
			}
		}
	}

    //cout<<endl<<out;
	return out;
}

string deformater(string in){   //ADD BACK SPACES TO COMMANDS FOR OUTPUT FILE
    string out="";

    string temp_j="";
    bool isJump=false;

    if(in=="jr$ra"){
        out="jr $ra";
        return out;
    }

    if(in[0]=='j'){
        for(int i=0; i<label_count; i++){//label matching
            temp_j="";
            temp_j+='j';
            for(int j=0; j<label_nm[i].length(); j++){
                temp_j+=label_nm[i][j];
            }
            if(in==temp_j){//is jump
                isJump=true;
            }
        }

        if(isJump){
            out+='j';
            out+=' ';
            for(int i=1; i<in.length(); i++){
                out+=in[i];
            }
            return out;
        }else{
            out+='j';
            out+='a';
            out+='l';
            out+=' ';
            for(int i=3; i<in.length(); i++){
                out+=in[i];
            }
            return out;
        }
    }


    if(opcode=="add" || opcode=="addu"  || opcode=="and" || opcode=="nor" || opcode=="or" || opcode=="slt" || opcode=="sltu" || opcode=="sub" || opcode=="subu"){ //R-TYPE
        for(int i=0; i<in.length(); i++){
            out+=in[i];
            if(in[i+1]=='$'){
                out+=' ';
            }
        }
        return out;
    }

    if(opcode=="addi" || opcode=="addui" || opcode=="addiu" || opcode=="andi" || opcode=="ori" || opcode=="slti" || opcode=="sltiu" || opcode=="sltui" || opcode=="sll" || opcode=="srl" || opcode=="beq" || opcode=="bne"){ //I/B-TYPE
        int comma=0, dol=0;
        for(int i=0; i<in.length(); i++){
            out+=in[i];
            if(in[i+1]=='$'){
                out+=' ';
                dol++;
            }
            if(in[i]==','){
                comma++;
                if(comma==2){
                    out+=' ';
                }
            }
        }
        return out;
    }

    if(opcode=="lw" || opcode=="sw" || opcode=="lui"){ //SPECIAL-I-TYPE & LUI
        int i=0;
        while(in[i+1]!='$'){
            out+=in[i];
            i++;
        }
        out+=in[i];
        i++;
        out+=' ';
        while(in[i]!=','){
            out+=in[i];
            i++;
        }
        out+=in[i];//comma
        i++;
        out+=' ';
        while(i<in.length()){
            out+=in[i];
            i++;
        }
        return out;
    }

    exit(-7);
    return NULL;
}

int starter(){ //RETURNS THE LINE NUMBER OF THE COMMAND AFTER "main:"
	int line=0;
	string com;
	string temp;

	while(1){
		com=parser(line);
		temp="";

		if(com.length() >= 5){
			for(int i=0; i<5; i++){
				temp+=com[i];
			}
			if(temp=="main:"){
				return line++;
			}
		}

		line++;
	}
}

void submit_label(string name, int pointer){    //ADDS LABEL NAME AND LINE POINTER TO THE LABEL ARRAYS
    if(label_count==lbl_max){
        exit(-6);
    }

    label_nm[label_count]=name;
    LBL[label_count]=pointer;
    label_count++;
}

void preparser(){      //PREPARSER CREATES A SEPARATE INSTRUCTION FILE WITHOUT EMPTY LINES, LABELS, SPACES, TABS, COMMENTS
    ofstream fout;
    fout.open("instr.txt");
    int i=0;
    string line;
    bool isLabel;
    int empt=0;

    if (!fout.is_open()) {
		exit(-5);
	}

    int mainline=starter();
    for(i=0; i<=mainline; i++){
        fout<<parser(i)<<endl;
    }

    do{
        isLabel=false;
        line=formater(parser(i));
        if(line!=""){
            if(line[line.length()-1]==':'){
                string lbl_bd="";
                int lbl_ln=i-empt-mainline-1-label_count;
                for(int j=0; j<line.length()-1; j++){
                    lbl_bd+=line[j];
                }
                submit_label(lbl_bd, lbl_ln);
            }else{
                fout<<line<<endl;
            }
        }else{
            empt++;
        }
        i++;
    }while(!EOF_flag);

    file_name="instr.txt";
    fout.close();
}

int reg_trans(string alias) {   //TRANSLATES REGISTER ALIAS TO REGISTER NUMBER
    for (int i = 0; i < 32; i++) {
        if (mipsRegisters[i] == alias) {
            return i;
        }
    }
    return -1; // Return -1 if the alias is not found
}

void j_type(string com){    //PROCESSES J TYPE COMMANDS
	string address_str="";
	string rs_str="";
    string label_str="";
    type="J";
    string temp_j="";
    bool isJump=false;

    if(com=="jr$ra"){
        rs_str="ra";
        rs=reg_trans("ra");
        address_str=intToHex(reg[reg_trans(rs_str)]);
        address=reg[reg_trans("ra")];
        opcode="jr";
        rt=0;
        rd=0;
        return;
    }

    for(int i=0; i<label_count; i++){//label matching
        temp_j="";
        temp_j+='j';
        for(int j=0; j<label_nm[i].length(); j++){
            temp_j+=label_nm[i][j];
        }
        if(com==temp_j){//is jump
            isJump=true;
        }
    }

    if(isJump){
        for(int i=1; i<com.length(); i++){
            label_str+=com[i];
        }
        opcode="j";
    }else{
        for(int i=3; i<com.length(); i++){
            label_str+=com[i];
        }
        //reg[reg_trans("ra")]=PC; //save $ra
        opcode="jal";
    }


	//rs=reg_trans(rs_str); ENABLE IF JR
	address=LBL[lbl_trans(label_str)];
	rs=32;
	rt=32;
	rd=32;
}

void r_type(string com){    //PROCESSES R TYPE COMMANDS
	int i;
	string rs_str="";
	string rt_str="";
	string rd_str="";
	type="R";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rd start
	do{
		rd_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rd done
	i++;
	i++;
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rs done
	i++;
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(i<com.length());

	rs=reg_trans(rs_str);
	rt=reg_trans(rt_str);
	rd=reg_trans(rd_str);
}

void b_type(string com){    //PROCESSES B TYPE COMMANDS
    int i;
	string rs_str="";
	string rt_str="";
	string label_str="";
	type="B";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rt done
	i++;
	i++;
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rs done
	i++;
	//label start
	do{
		label_str+=com[i];
		i++;
	}while(i<com.length());

	rs=reg_trans(rs_str);
	rt=reg_trans(rt_str);
	target=LBL[lbl_trans(label_str)];
	//cout<<endl<<target<<endl;
	rd=32;
}

void i_type(string com){    //PROCESSES I TYPE COMMANDS
    int i;
	string rs_str="";
	string rt_str="";
	string immed_str="";
	type="I";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rt done
	i++;
	i++;
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rs done
	i++;
	//immed start
	do{
		immed_str+=com[i];
		i++;
	}while(i<com.length());

	rs=reg_trans(rs_str);
	rt=reg_trans(rt_str);
	if(immed_str[0]=='0' && immed_str[1]=='x'){
        string temp="";
        for(int j=2; j<=immed_str.length(); j++){
            temp+=immed_str[j];
        }
        immed=hex_dec(temp);
        unsigned_immed=directToInt(temp);
	}else{
        immed=stringToInt(immed_str);
	}
	rd=32;
}

void special_i_type(string com){    //PROCESSES SPECIAL I TYPE COMMANDS
    int i;
	string rs_str="";
	string rt_str="";
	string immed_str="";
	type="SPI";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rt done
	i++;//skip comma

    if(opcode=="lui"){   //SPECIAL DECODING
        for(int j=i; j<com.length(); j++){
            immed_str+=com[j];
        }
        rt=reg_trans(rt_str);//ON PURPOSE
        if(immed_str[0]=='0' && immed_str[1]=='x'){
            string temp="";
            for(int k=2; k<immed_str.length(); k++){
                temp+=immed_str[k];
            }
            immed=hex_dec(temp);
        }else{
            immed=stringToInt(immed_str);
        }
        return;
    }

	while(com[i]>='0' && com[i]<='9'){
        immed_str+=com[i];
        i++;
	}
	//immed done
	i++;//skip parentheses
	i++;//skip $
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=')');

	if(rs_str=="pc"){
        rs=PC+1;                                            //MAYBE REMOVE +1 FOR MC
	}else{
        rs=reg_trans(rs_str);
	}
	rt=reg_trans(rt_str);
	immed=stringToInt(immed_str);
	rd=32;
}

void instr_reg(int line){   //TAKES COMMAND AND OUTPUTS THE PROPER FIELDS DEPENDING ON THE COMMAND TYPE
	string com=parser(line);
	instr=com;
	opcode="";
	int i=0;

	cout<<com<<endl;

	if(com[0]=='j'){ //J-TYPE
		j_type(com);
	}else{
		i=0;
		do{
			opcode+=com[i];
			i++;
		}while(com[i]!='$');
		//I have opcode in string format;

		if(opcode=="add" || opcode=="addu"  || opcode=="and" || opcode=="nor" || opcode=="or" || opcode=="slt" || opcode=="sltu" || opcode=="sub" || opcode=="subu"){ //R-TYPE
			r_type(com);
		}else{
			if(opcode=="addi" || opcode=="addui" || opcode=="addiu" || opcode=="andi" || opcode=="ori" || opcode=="slti" || opcode=="sltiu" || opcode=="sltui" || opcode=="sll" || opcode=="srl"){ //I-TYPE
				i_type(com);
			}else{
			    if(opcode=="sw" || opcode=="lw" || opcode=="lui"){ //SPECIAL-I-TYPE
                    special_i_type(com);
                }else{
                    if(opcode=="beq" || opcode=="bne"){ //B-TYPE
                        b_type(com);
                    }else{
                        exit(-2);
                    }
                }
			}
		}
	}
	//theoretically have opcode, rs, rt, rd, func, immed, address
}

int target_calc(){ //CALCULATES BRANCH TARGET
	//target=PC+immed;//not *4 bcs addresses go +1
	return target;
}

void PC_Writer(){
    //cout<<pc_write<<" "<<pc_cond<<" "<<zero<<" "<<opcode<<endl;
    if(pc_write || (pc_cond && zero && opcode=="beq") || (pc_cond && !zero && opcode=="bne")){
        //cout<<PC<<"  ";
        if(pc_source==0){
            PC=aluout;
        }else{
            if(pc_source==1){
                PC=target;
            }else{
                if(pc_source==2){
                    PC=address;
                }else{
                    PC=reg[reg_trans("ra")];
                }
            }
        }
        //cout<<PC<<endl;
    }
}


void control_unit(){    //SETS CONTROLL SIGNALS

    if(instr=="sll$zero,$zero,0"){
        end_time=clock();
        userTime=static_cast<double>(end_time-start_time)/CLOCKS_PER_SEC;
        print_monitors(true);
		exit(0);
	}

	switch (FSM){
        case 1:
            instr_reg(mainline+PC);//FETCH
            ALUsrcA=0;
            ALUsrcB=1;
            aluop=2;
            pc_write=1;
            pc_cond=0;
            iord=0;
            memread=1;
            memwrite=0;
            memtoreg=0;
            irwrite=1;
            pc_source=0;
            regwrite=0;
            regdst=1;
            uns=0;
            FSM=2;
        break;

        case 2:
            reg_file();//DECODE
            ALUsrcA=1;
            ALUsrcB=2;
            aluop=2;
            pc_write=0;
            pc_cond=0;
            iord=0;
            memread=0;
            memwrite=0;
            memtoreg=0;
            irwrite=0;
            pc_source=0;
            regwrite=0;
            regdst=1;
            uns=0;
            FSM=3;
        break;

        case 3://EXECUTE

            //target=aluout; //because target=label

            if(opcode=="add"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="addi"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            if(opcode=="addui" || opcode=="addiu"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=1;
                FSM=4;
            }

            if(opcode=="addu"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;//MAYBE CHANGE
                FSM=4;
            }

            if(opcode=="and"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=0;//and
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="andi"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=0;//and
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            if(opcode=="sub" || opcode=="subu"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=6;//sub
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="beq" || opcode=="bne"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=6;//sub
                pc_write=0;
                pc_cond=1;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=1;//target
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=1;
                //cout<<reg[rs]<<"   "<<reg[rt]<<endl;
            }

            if(opcode=="lw" || opcode=="sw"){//mem addr calc
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;//add
                pc_write=0;
                pc_cond=0;
                iord=1;
                memread=0;
                memwrite=0;
                memtoreg=1;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            if(opcode=="nor"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=12;//nor
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="or"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=1;//nor
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="ori"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=1;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            if(opcode=="slt"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=3;//slt
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="slti"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=3;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=4;
            }

            if(opcode=="sltui" || opcode=="sltiu"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=3;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=1;
                FSM=4;
            }

            if(opcode=="sltu"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=8;//slt
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=1;
                uns=1;
                FSM=4;
            }

            if(opcode=="sll"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=5;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            if(opcode=="srl"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=4;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            if(opcode=="j"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=2;
                pc_write=1;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=2;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="jal"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=2;
                pc_write=1;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=2;
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=1;
                reg[reg_trans("ra")]=4*(PC-1);
            }

            if(opcode=="jr"){
                ALUsrcA=1;
                ALUsrcB=1;
                aluop=2;
                pc_write=1;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;//alu
                regwrite=0;
                regdst=1;
                uns=0;
                FSM=1;
                //PC=reg[reg_trans("ra")];
            }

            if(opcode=="lui"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=7;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;//$ra
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=4;
            }

            //ALU();

        break;

        case 4:
            if(opcode=="add"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="addi"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="addui" || opcode=="addiu"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=1;
                FSM=1;
            }

            if(opcode=="addu"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=2;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;//MAYBE CHANGE
                FSM=1;
            }

            if(opcode=="and"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=0;//and
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="andi"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=0;//and
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="sub" || opcode=="subu"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=6;//sub
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="lw"){//mem addr calc
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;//add
                pc_write=0;
                pc_cond=0;
                iord=1;
                memread=1;
                memwrite=0;
                memtoreg=1;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=5;
            }

            if(opcode=="sw"){//mem addr calc
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;//add
                pc_write=0;
                pc_cond=0;
                iord=1;
                memread=0;
                memwrite=1;
                memtoreg=1;
                irwrite=0;
                pc_source=0;
                regwrite=0;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="nor"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=12;//nor
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="or"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=1;//nor
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="ori"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=1;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="slt"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=3;//slt
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=0;
                FSM=1;
            }

            if(opcode=="slti"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=3;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="sltui" || opcode=="sltiu"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=3;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=1;
                FSM=1;
            }

            if(opcode=="sltu"){
                ALUsrcA=1;
                ALUsrcB=0;
                aluop=8;//slt
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=1;
                uns=1;
                FSM=1;
            }

            if(opcode=="sll"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=5;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="srl"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=4;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

            if(opcode=="lui"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=7;
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=0;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }

        break;

        case 5:
            if(opcode=="lw"){
                ALUsrcA=1;
                ALUsrcB=2;
                aluop=2;//add
                pc_write=0;
                pc_cond=0;
                iord=0;
                memread=0;
                memwrite=0;
                memtoreg=1;
                irwrite=0;
                pc_source=0;
                regwrite=1;
                regdst=0;
                uns=0;
                FSM=1;
            }
        break;
	}

	ALU();


}

void reg_file(){    //OUTPUTS ALU OPERANTS AND UPDATES REGISTER DATA
	readA=reg[rs];
	readB=reg[rt];
	mem_in=readB;

	if(memtoreg){
		write_data=mem_out;
	}else{
		write_data=aluout;
	}

	if(regdst){
		wr=rd;//write register
	}else{
		wr=rt;
	}

	if(regwrite){
		reg[wr]=write_data;
	}
}

void ALU(){     //EXECUTES ARITHMETIC AND LOGICAL OPERATIONS
	int inA;
	int inB;
    aluout=0;
    int temp1=0, temp2=0;

	if(!ALUsrcA){
		inA=PC;
	}else{
		inA=readA;
	}

	switch(ALUsrcB){
        case 0:
            inB=readB;
            if(uns){
                //cout<<readB<<"   "<<intToHex(readB)<<"   "<<directToInt(intToHex(readB))<<endl;
                inB=directToInt(intToHex(readB));
            }
        break;

        case 1:
            inB=1;
        break;

        case 2:
            inB=immed;
            if(uns){
                //cout<<inB<<"   "<<intToHex(readB)<<"   "<<directToInt(intToHex(readB))<<endl;
                inB=directToInt(intToHex(immed));
            }
        break;

        case 3:
            inB=immed*1;    //MAYBE NEEDED TO MAKE PC INCREMENT BY 4
        break;
	}

    glinA=inA;
    glinB=inB;

	switch(aluop){

		case 2://add
			aluout=inA+inB;
		break;

		case 6://sub
			aluout=inA-inB;
		break;

		case 7://lui
		    temp1=reg[rt];
		    temp2=temp1;
		    temp1=temp1%(int)pow(16,4);
		    temp2 = static_cast<uint32_t>(inB) << 16;
			aluout=temp2 + temp1;
		break;

		case 3://slt
			aluout=inA-inB;
			if(aluout<0)
                aluout=1;
            else
                aluout=0;
		break;

		case 8://sltu
			aluout=directToInt(intToHex(inA))-directToInt(intToHex(inB));
			if(aluout<0)
                aluout=1;
            else
                aluout=0;
		break;

		case 0://and
			aluout=inA & inB;
		break;

		case 1://or
			aluout=inA | inB;
		break;

		case 12://nor
			aluout=~(inA | inB);
		break;

		case 5://sll
			aluout=inA;
			for(int i=0; i<inB; i++){
				aluout*=2;
			}
		break;

		case 4://srl
			aluout=inA;
			for(int i=0; i<inB; i++){
				aluout/=(int)2;
			}
		break;
	}

	if(aluout==0){
		zero=1;
	}else{
		zero=0;
	}

	if(opcode=="jr" && FSM==1){
        aluout=((aluout-1)/4)+1;
	}
}

long msbPC(){   //TAKES THE 4 MSB FROM PC === deprecated
	long msb=PC & (~(268435455));
	//cout<<endl<<msb<<endl;
	return msb;
}

void jmp(){ //SETS PC TO JUMP ADDRESS
	if(jump){
		PC=address; // was addr+msbPC()
	}
}

int lbl_trans(string label){    //TRANSLATES LABELS ALIAS TO LABEL NUMBER
    for(int i=0; i<lbl_max; i++){
        if(label_nm[i]==label){//search for existing label
            return i;
        }
    }
    return -1;
}

int mem_trans(unsigned int addr){   //TRANSLATES MEMORY ADDRESS TO MEMORY DATA AND ASSIGNS NEW MEMORY SPACES
    for(int i=0; i<mmp; i++){
        if(memAddress[i]==addr){//search for existing mem address
            return i;
        }
    }
    //else
    for(int i=0; i<mmp; i++){//assign new mem address
        if(memAddress[i]==0){
            memAddress[i]=addr;
            return i;//contents in MEM[i] will be zero since it is the first call
        }
    }
    exit(-3);//OUT OF MEMORY SPACES
}

void memory(){  //INPUTS OR OUTPUTS DATA FROM MEMORY
	if(memread){
		mem_out=MEM[mem_trans(aluout)];
	}

	if(memwrite){
		MEM[mem_trans(aluout)]=mem_in;
	}
}

void fillMem(){ //INITIALIZES MEMORY AND REGISTERS
    reg[reg_trans("gp")]=hex_dec("10008000");
    reg[reg_trans("sp")]=hex_dec("7FFFFFFC");
}

string aluop_trans(int op){ //translates internal aluop to actual mips aluop
    switch(op){
        case 0:
            return "0010";
        break;

        case 1:
            return "0001";
        break;

        case 2:
            return "0000";
        break;

        case 3:
            return "0111";
        break;

        case 4:
            return "0010";//dc
        break;

        case 5:
            return "0010";//dc
        break;

        case 6:
            return "0110";
        break;

        case 7:
            return "0001";
        break;

        case 8:
            return "0111";
        break;

        case 12:
            return "1100";
        break;

        default:
            return "0010";
        break;
    }
    return "0010";
}

void print_monitors(bool endd){ //PRINTS MONITORS for debug
    ofstream fout;
    fout.open("output.txt", ios::app);
    if (!fout.is_open()) {
		exit(-4);
	}

	if(endd){
        fout<<endl<<"-----Final State-----"<<endl;
	}else{
        fout<<endl<<"-----Cycle "<<cycles<<"-----"<<endl;
	}
	fout<<"Registers:"<<endl;
	if(endd)
        fout<<trim(intToHex(PC*4-4))<<'\t';
    else
        fout<<trim(intToHex(PC*4))<<'\t';
	for(int i=0; i<32; i++){
        if(reg[i]==0){
            fout<<"0"<<'\t';
        }else{
            fout<<trim(intToHex(reg[i]))<<'\t';
        }
	}


	if(!endd){
        fout<<endl<<endl<<"Monitors:"<<endl;
        //fout<<endl<<type<<endl;
        if(FSM!=2)
            fout<<trim(intToHex(PC*4))<<'\t';//mon1      //<<deformater(instr)<<'\t';
        else
            fout<<trim(intToHex(PC*4-4))<<'\t';

        if(iord==0){//mon 2
            fout<<trim(intToHex(PC*4-4))<<'\t';
        }else{
            fout<<trim(intToHex(aluout))<<'\t';
        }

        if(opcode=="sw") fout<<trim(intToHex(prevB))<<'\t'; else fout<<"-"<<'\t';//mon3

        if(iord==1 && memread){//mon 4
            fout<<trim(intToHex(MEM[mem_trans(aluout)]))<<'\t';
        }else{
            fout<<trim(deformater(instr))<<'\t';
        }

        fout<<opcode<<'\t';//mon 5

        if(rs!=32) fout<<"$"<<mipsRegisters[rs]<<'\t'; else fout<<"-"<<'\t';//mon 6

        if(rt!=32) fout<<"$"<<mipsRegisters[rt]<<'\t'; else fout<<"-"<<'\t';//mon 7

        fout<<trim(intToHex(immed))<<'\t';//mon 8

        if(opcode=="lw") fout<<trim(intToHex(mem_out))<<'\t'; else fout<<"-"<<'\t';//mon 9

        if(rs!=32) fout<<"$"<<mipsRegisters[rs]<<'\t'; else fout<<"-"<<'\t';//mon 10

        if(rt!=32) fout<<"$"<<mipsRegisters[rt]<<'\t'; else fout<<"-"<<'\t';//mon 11

        if(wr!=32) fout<<"$"<<mipsRegisters[wr]<<'\t'; else fout<<"-"<<'\t';//mon 12

        if(type=="J" || type=="B") fout<<"-"<<'\t'; else fout<<trim(intToHex(write_data))<<'\t';//mon 13

        fout<<trim(intToHex(readA))<<'\t';//mon 14

        fout<<trim(intToHex(readB))<<'\t';//mon 15

        fout<<trim(intToHex(prevA))<<'\t';//mon 16

        fout<<trim(intToHex(prevB))<<'\t';//mon 17

        fout<<trim(intToHex(glinA))<<'\t';//mon 18

        fout<<trim(intToHex(glinB))<<'\t';//mon 19

        fout<<trim(intToHex(aluout))<<'\t';//mon 20

        fout<<trim(intToHex(prevALUout))<<'\t';//mon 21

        fout<<pc_cond<<'\t'<<pc_write<<'\t'<<iord<<'\t'<<memread<<'\t'<<memwrite<<'\t'<<memtoreg<<'\t'<<irwrite<<'\t'<<trim(intToBinary(pc_source))<<'\t'<<aluop_trans(aluop)<<'\t'<<trim(intToBinary(ALUsrcB))<<'\t'<<ALUsrcA<<'\t'<<regwrite<<'\t'<<regdst;
	}

	fout<<endl<<endl<<"Memory State:"<<endl;

	if(opcode=="sw" && FSM==1){//last cycle of fsm
        fout<<trim(intToHex(MEM[mem_trans(aluout)]));
	}

    if(endd){
        for(int i=0; i<mmp; i++){
            if(memAddress[i]!=0 && MEM[i]!=0){
                fout<<trim(intToHex(MEM[i]))<<'\t';
            }
        }
    }

    fout<<endl;
    if(endd){
        fout<<endl<<"Total Cycles:"<<endl<<cycles-2;
        fout<<endl<<endl<<"User Time: "<<endl<<userTime*1000000000;//<<" ns";
    }

	fout.close();
}

void monitors(){    //CHECKS WHETHER IT SHOULD PRINT MONITORS BASES ON CURRENT CYCLE
    if(cycles==cyc_to_print[cyc_count]){
        cyc_count++;
        print_monitors(false);
    }else{
        if(cyc_to_print[cyc_count]==0){
            print_monitors(false);
        }
    }
}

void init_out(){    //INITIALISES OUTPUT FILE
    ofstream fout;
    fout.open("output.txt");
    if (!fout.is_open()) {
		exit(-4);
	}
	fout<<"Name: MCH170"<<endl;
	fout.close();
}

void brnch(){   //SETS PC TO BRANCH TARGET ADDRESS - REPLACED BY PC_Writer
    //target=2;
    if(branch && zero && opcode=="beq"){
        PC=target;
    }
    if(branch && !zero && opcode=="bne"){
        PC=target;
    }
}

void reg_shifter(){
    prevA=readA;
    prevB=readB;
    prevALUout=aluout;
}

int main(int argc, char *argv[])
{

    int i=0;

	cout<<"Command file name: ";
    cin>>file_name;
    cout<<"Cycles to print in ascending order (-1 to continue, 0 for all): "<<endl;
    do{
        cin>>cyc_to_print[i];
        i++;
    }while(cyc_to_print[i-1]!=-1 && cyc_to_print[i-1]!=0);

    start_time=clock();
    preparser();
    init_out();
    fillMem();
	mainline=starter()+1;

	int first=1;
	while(1){
        switch(FSM){
            case 1:
                //if(!first && 0)
                //    print_monitors(0);
                first=0;
                control_unit();
                PC_Writer();
            break;

            case 2:
                control_unit();
            break;

            case 3:
                control_unit();
                PC_Writer();
            break;

            case 4:
                control_unit();
                reg_file();
                memory();
            break;

            case 5:
                control_unit();
                reg_file();
            break;
        }
        //_sleep(50);
        reg_file();
        memory();
        monitors();
        reg_shifter();//maybe before monitors
        cycles++;
	}

    print_monitors(true);
    return 0;
}

//ERROR MESSAGES
//0 ALL GOOD - 1 INPUT FILE ERROR - 2 INVALID COMMAND - 3 OUT OF MEMORY - 4 OUTPUT FILE ERROR - 5 PREPARSER OUTPUT FILE ERROR - 6 OUT OF LABEL SPACE - 7 DEFORMATER UNKNOWN TYPE
