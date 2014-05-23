/*
 * Copyright (c) 2014, Alan L. Lovejoy
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation are those
 * of the authors and should not be interpreted as representing official policies, 
 * either expressed or implied, of the Essence Sharp Project.
*/

#region Using declarations
using System;
using System.IO;
using System.Text;
#endregion

namespace EssenceSharp {
	
	#region Enumeration Types

	public enum CapitalizationScheme {
		AsIs,
		InitialCapital,
		InitialLowerCase
	}

	public enum SymbolType {
		Identifier,
		Keyword,
		BinaryMessageSelector,
		String
	}

	#endregion
	
	#region Root Object
	
	public abstract class EssenceSharpObject {

		public virtual void printTypeUsing(Action<String> append) {
			append(GetType().Name);
		}
		
		public virtual void printUsing(Action<String> append, Action<uint> newLine) {
			printUsing(0, append, newLine);
		}
		
		public virtual void printUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append("{<");
			printTypeUsing(append);
			append(">");
			printElementsUsing(depth+1, append, newLine);
			append("}");
		}
		
		public virtual void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
		}
		
		protected virtual void printUsing(EssenceSharpObject[] elements, uint depth, Action<String> append, Action<uint> newLine) {
			if (elements != null) {
				for (uint i = 0; i < elements.Length; i++) {
					newLine(depth);
					elements[i].printUsing(depth, append, newLine);
				}
			}
		}

		public virtual void printOn(StringBuilder sb) {
			printUsing(
				text => sb.Append(text),
				delegate (uint depth) {
					sb.AppendLine("");
					if (depth > 0) {
						sb.Append(new String(' ', (int)depth * 8));
					}
				});
		}
		
		public virtual void printOn(TextWriter stream) {
			printUsing(
				stream.Write,
				delegate (uint depth) {
					stream.WriteLine("");
					if (depth > 0) {
						stream.WriteLine(new String(' ', (int)depth * 8));
					}
				});
		}
		
		public override String ToString() {
			var sb = new StringBuilder();
			printOn(sb);
			return sb.ToString();		
		
		}
		
	}

	#endregion
	
	#region Delegate Types
	
	#region Heterogenous Parameter Type Functors

	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T, P32T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31, P32T p32);
	public delegate RT Func<P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T, P32T, P33T, RT>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31, P32T p32, P33T p33);

	public delegate RT Functor0<RT>();
	public delegate RT Functor1<RT, P1T>(P1T p1);
	public delegate RT Functor2<RT, P1T, P2T>(P1T p1, P2T p2);
	public delegate RT Functor3<RT, P1T, P2T, P3T>(P1T p1, P2T p2, P3T p3);
	public delegate RT Functor4<RT, P1T, P2T, P3T, P4T>(P1T p1, P2T p2, P3T p3, P4T p4);
	public delegate RT Functor5<RT, P1T, P2T, P3T, P4T, P5T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5);
	public delegate RT Functor6<RT, P1T, P2T, P3T, P4T, P5T, P6T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6);
	public delegate RT Functor7<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7);
	public delegate RT Functor8<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8);
	public delegate RT Functor9<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9);
	public delegate RT Functor10<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10);
	public delegate RT Functor11<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11);
	public delegate RT Functor12<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12);
	public delegate RT Functor13<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13);
	public delegate RT Functor14<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14);
	public delegate RT Functor15<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15);
	public delegate RT Functor16<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16);
	public delegate RT Functor17<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17);
	public delegate RT Functor18<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18);
	public delegate RT Functor19<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19);
	public delegate RT Functor20<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20);
	public delegate RT Functor21<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21);
	public delegate RT Functor22<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22);
	public delegate RT Functor23<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23);
	public delegate RT Functor24<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24);
	public delegate RT Functor25<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25);
	public delegate RT Functor26<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26);
	public delegate RT Functor27<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27);
	public delegate RT Functor28<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28);
	public delegate RT Functor29<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29);
	public delegate RT Functor30<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30);
	public delegate RT Functor31<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31);
	public delegate RT Functor32<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T, P32T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31, P32T p32);
	public delegate RT Functor33<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T, P32T, P33T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31, P32T p32, P33T p33);
	public delegate RT Functor34<RT, P1T, P2T, P3T, P4T, P5T, P6T, P7T, P8T, P9T, P10T, P11T, P12T, P13T, P14T, P15T, P16T, P17T, P18T, P19T, P20T, P21T, P22T, P23T, P24T, P25T, P26T, P27T, P28T, P29T, P30T, P31T, P32T, P33T, P34T>(P1T p1, P2T p2, P3T p3, P4T p4, P5T p5, P6T p6, P7T p7, P8T p8, P9T p9, P10T p10, P11T p11, P12T p12, P13T p13, P14T p14, P15T p15, P16T p16, P17T p17, P18T p18, P19T p19, P20T p20, P21T p21, P22T p22, P23T p23, P24T p24, P25T p25, P26T p26, P27T p27, P28T p28, P29T p29, P30T p30, P31T p31, P32T p32, P33T p33, P34T p34);

	#endregion
	
	#region Homegenous Parameter Type Functors
	
	public delegate T Functor1<T>(T p1);
	public delegate T Functor2<T>(T p1, T p2);
	public delegate T Functor3<T>(T p1, T p2, T p3);
	public delegate T Functor4<T>(T p1, T p2, T p3, T p4);
	public delegate T Functor5<T>(T p1, T p2, T p3, T p4, T p5);
	public delegate T Functor6<T>(T p1, T p2, T p3, T p4, T p5, T p6);
	public delegate T Functor7<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7);
	public delegate T Functor8<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8);
	public delegate T Functor9<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9);
	public delegate T Functor10<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10);
	public delegate T Functor11<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11);
	public delegate T Functor12<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12);
	public delegate T Functor13<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13);
	public delegate T Functor14<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14);
	public delegate T Functor15<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15);
	public delegate T Functor16<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16);
	public delegate T Functor17<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17);
	public delegate T Functor18<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18);
	public delegate T Functor19<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19);
	public delegate T Functor20<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20);
	public delegate T Functor21<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21);
	public delegate T Functor22<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22);
	public delegate T Functor23<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23);
	public delegate T Functor24<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24);
	public delegate T Functor25<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25);
	public delegate T Functor26<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26);
	public delegate T Functor27<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27);
	public delegate T Functor28<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28);
	public delegate T Functor29<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28, T p29);
	public delegate T Functor30<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28, T p29, T p30);
	public delegate T Functor31<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28, T p29, T p30, T p31);
	public delegate T Functor32<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28, T p29, T p30, T p31, T p32);
	public delegate T Functor33<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28, T p29, T p30, T p31, T p32, T p33);
	public delegate T Functor34<T>(T p1, T p2, T p3, T p4, T p5, T p6, T p7, T p8, T p9, T p10, T p11, T p12, T p13, T p14, T p15, T p16, T p17, T p18, T p19, T p20, T p21, T p22, T p23, T p24, T p25, T p26, T p27, T p28, T p29, T p30, T p31, T p32, T p33, T p34);

	#endregion

	#endregion

}




