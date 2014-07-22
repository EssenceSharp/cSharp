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
using System.Text;
using System.Collections.Generic;
using System.IO;
using System.Globalization;
using System.Reflection;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.UtilityServices {

	/// <summary>
	/// TypeName parses and represents CLR type names as used by the CLR reflection library, and does not accept or produce the names of Essence# types, nor accept or produce 
	/// the names of types as used by the MSIL, nor accept or produce the type name syntax of any particular CLR-based programming language.
	/// </summary>
	public class TypeName {

		#region Static variables and methods

		public static readonly byte[] nullPublicKeyOrKeyToken = new byte[0];

		public static void parseQualifiedNameFromString(String typeName, out List<String> namespaceElements, out List<String> containingTypes, out String namePrefix, out int genericArity, out TypeName[] genericArguments, out bool isArray) {
			if (String.IsNullOrEmpty(typeName)) throw new PrimInvalidOperandException("Type name must not be empty or null.");
			parseQualifiedNameFromStream(new StringReader(typeName), out namespaceElements, out containingTypes, out namePrefix, out genericArity, out genericArguments, out isArray);
		}

		public static void parseQualifiedNameFromStream(TextReader stream, out List<String> namespaceElements, out List<String> containingTypes, out String namePrefix, out int genericArity, out TypeName[] genericArguments, out bool isArray) {
			// Leading whitespace is accepted.
			// The namespace, any containing types and the assembly name may be omitted.
			// Generic types must at least specify their parameter arity, but may omit any type arguments.
			// And don't blame me for the complexity of the code required to parse type name syntax. The following is what's required by what Microsoft hath wrought:

			var element = stream.nextIdentifier();
			if (element == null) throw new PrimInvalidOperandException("Expecting type name, but encountered end of input."); 
			if (element.Length < 1) throw new PrimInvalidOperandException("Type name must start with a letter or underscore."); 

			namespaceElements			= null;
			containingTypes				= null;
			namePrefix				= element;
			genericArity				= 0;
			genericArguments			= null;
			isArray					= false;
			
			String		outerTypeElement	= element;

			var		c			= stream.Peek();
			var		ch			= c >= 0 ? (char)c : (char)0;

			if (ch == '.' || ch == '+') {

				namespaceElements = new List<String>();
				while (stream.nextMatches('.')) {
					if (element.Length < 1) {
						var prefix = namespaceElements.ToArray().compose(".");
						throw new PrimInvalidOperandException("Namespace path element cannot have a length of zero. Check for unintentional duplication of the separator character ('.'). Prefix = " + prefix);
					}
					namespaceElements.Add(element);
					element = stream.nextIdentifier();
					if (element == null) {
						var prefix = namespaceElements.ToArray().compose(".");
						throw new PrimInvalidOperandException("Type name cannot end with a period. Prefix = " + prefix);
					}
					outerTypeElement = element;
				}

				if (stream.nextMatches('`')) {
					var nArity = stream.nextUnsignedInteger();
					if (nArity == null) {
						var prefix = namespaceElements.ToArray().compose(".");
						throw new PrimInvalidOperandException("Type name cannot end with a '`' (backquote character.) Prefix = " + prefix);
					}
					genericArity = (int)nArity;
				} 

				namePrefix = outerTypeElement;

				c = stream.Peek();
				ch = c >= 0 ? (char)c : (char)0;

				if (ch == '+') {

					var containingType = outerTypeElement;
					containingTypes = new List<String>();
					while (stream.nextMatches('+')) {
						if (genericArity > 0) {
							containingType = containingType + "`" + genericArity.ToString();
						}
						containingTypes.Add(containingType);
						var nsElements = namespaceElements;	// Can't reference out parameters in a lambda
						var containers = containingTypes;	// Can't reference out parameters in a lambda
						if (parseUnqualifiedName(
							stream, 
							out containingType, 
							out genericArity, 
							(localPrefix, errorDescription) => {
								var prefix = nsElements.ToArray().compose(".");
								prefix = prefix + "." + containers.ToArray().compose("+");
								if (localPrefix == null || localPrefix.Length < 1) {
									throw new PrimInvalidOperandException("Nested type element cannot have a length of zero. Check for unintentional duplication of the separator character ('+'). Prefix = " + prefix);
								} else {
									throw new PrimInvalidOperandException("Nested type name cannot end with a '`' (backquote character.). Prefix = " + prefix);
								}
							})) {
							namePrefix = containingType;
						}
					}

				} 

			} else if (stream.nextMatches('`')) {
				var nArity = stream.nextUnsignedInteger();
				if (nArity == null) {
					throw new PrimInvalidOperandException("Type name cannot end with a '`' (backquote character.)");
				}
				genericArity = (int)nArity;
			} 

			if (stream.nextMatches('[')) {
				var args = new List<TypeName>();
				c = stream.Peek();
				ch = c >= 0 ? (char)c : (char)0;
				if (ch.isInitialIdentifierChar()) {
					args.Add(fromStream(stream));
				} else { 
					do {
						while (stream.nextMatches('[')) {
							args.Add(fromStream(stream));
							if (!stream.nextMatches(']')) {
								String prefix = "";
								if (namespaceElements != null) {
									prefix = namespaceElements.ToArray().compose(".") + ".";
								}
								if (containingTypes != null) {
									prefix = prefix + containingTypes.ToArray().compose("+") + "+";
								}
								prefix = prefix + namePrefix;
								throw new PrimInvalidOperandException("A generic type parameter in a type name must be terminated by a ']' character. Prefix = " + prefix);
							}
						}
					} while (stream.nextMatches(','));
				}
				if (!stream.nextMatches(']')) {
					String prefix = "";
					if (namespaceElements != null) {
						prefix = namespaceElements.ToArray().compose(".") + ".";
					}
					if (containingTypes != null) {
						prefix = prefix + containingTypes.ToArray().compose("+") + "+";
					}
					prefix = prefix + namePrefix;
					throw new PrimInvalidOperandException("A type name's list of generic type parameters must be terminated by a ']' character. Prefix = " + prefix);
				}
				if (genericArity > 0) {
					if (args.Count != genericArity) {
						String prefix = "";
						if (namespaceElements != null) {
							prefix = namespaceElements.ToArray().compose(".") + ".";
						}
						if (containingTypes != null) {
							prefix = prefix + containingTypes.ToArray().compose("+") + "+";
						}
						prefix = prefix + namePrefix;
						throw new PrimInvalidOperandException("The number of generic type parameters does not match the specified arity: " + prefix);
					}
				} else if (args.Count < 1) {
					isArray = true;
				} else {
					genericArity = args.Count;
				}
				genericArguments = args.ToArray();
			} 

		}

		public static bool parseUnqualifiedName(String unqualifiedTypeName, out String namePrefix, out int genericArity, System.Action<String, String> handleError) {
			// Leading whitespace is accepted.
			// Note: The arlgorithm leaves any generic type arguments unparsed. To parse generic type arguments, use the fromString(String) or fromStream(TextReader) methods.
			return parseUnqualifiedName(new StringReader(unqualifiedTypeName), out namePrefix, out genericArity, handleError);
		}

		public static bool parseUnqualifiedName(TextReader namePrefixStream, out String namePrefix, out int genericArity, System.Action<String, String> handleError) {
			// Leading whitespace is accepted.
			// Note: The arlgorithm leaves any generic type arguments unparsed. To parse generic type arguments, use the fromString(String) or fromStream(TextReader) methods.
			genericArity = 0;
			namePrefix = namePrefixStream.nextIdentifier();
			if (namePrefix == null) {
				if (handleError == null) {
					throw new PrimInvalidOperandException("Expecting type/namespace name, encountered end of input.");
				} else {
					handleError(null, "Expecting type/namespace name, encountered end of input.");
				}
				return false;
			}
			if (namePrefix.Length < 1) {
				if (handleError == null) {
					throw new PrimInvalidOperandException("Type or namespace name must start with a letter or underscore.");
				} else {
					handleError("", "Type/namespace name must start with a letter or underscore.");
				}
				return false;
			}
			if (namePrefixStream.nextMatches('`')) {
				var nArity = namePrefixStream.nextUnsignedInteger();
				if (nArity == null) {
					if (handleError == null) {
						throw new PrimInvalidOperandException("Type name must not end with a '`' (backquote character.) Prefix = " + namePrefix);
					} else {
						handleError(namePrefix, "Type name must not end with a '`' (backquote character.)");
					}
					return false;
				}
				genericArity = (int)nArity;
			}
			return true;
		}

		public static bool parseAssemblyName(TextReader stream, out AssemblyName assemblyName, System.Action<String, String> handleError) {
			/* Accepted/recognized syntax (using the ISO International Standard for EBNF; see the Wikipedia article for details):
			
				AssemblyName		= NamePrefix, [NameElementSeparator, PropertySpec, [NameElementSeparator, PropertySpec, [NameElementSeparator, PropertySpec, [NameElementSeparator, PropertySpec]]]];	
							  (* Note: It is a semantic error for the same PropertySpec to occur more than once,
								or for the PublicKeySpec and the PublicKeyTokenSpec to both occur. *)
				NameElementSeparator	= ',', [Whitespace];
				NamePrefix		= [Whitespace], Identifier,{".", Identifier};
				Identifier		= (Letter | '_'), {Letter | '_' | Digit};
				PropertySpec		= VersionSpec | CultureSpec | PublicKeySpec | PublicKeyTokenSpec; (* The Custom property is NOT supported *)
				VersionSpec		= 'Version', '=', 3 * (VersionNumberInteger, "."), VersionNumberInteger;
				CultureSpec		= 'Culture', '=', Identifier, {'-', Identifier}; (* The syntax as required by MS has additional restrictions on the identifiers, but this is what the implementation will recognize/accept *)
				PublicKeySpec		= 'PublicKey', '=', (HexadecimalDigit, {HexadecimalDigit}) | 'null';
				PublicKeyTokenSpec	= 'PublicKeyToken', '=', (16 * HexadecimalDigit) | 'null';
				CustomSpec		= 
				VersionNumberInteger	= '0' | (Digit - '0', {Digit}); (* Unsigned number from 0 to 65535 *)
				Digit			= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
				HexadecimalDigit	= Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f';
				Letter			= ? 'A'..'Z' | 'a'..'z' ?;
			*/

			assemblyName = new AssemblyName();
			var namePrefix = stream.nextQualifiedIdentifier();
			if (namePrefix == null) {
				if (handleError == null) {
					throw new PrimInvalidOperandException("Expecting assembly name, encountered end of input.");
				} else {
					handleError(null, "Expecting assembly name, encountered end of input.");
				}
				return false;
			}
			if (namePrefix.Length < 1) {
				if (handleError == null) {
					throw new PrimInvalidOperandException("Assembly name must start with a letter or underscore.");
				} else {
					handleError("", "Assembly name must start with a letter or underscore.");
				}
				return false;
			}

			assemblyName.Name = namePrefix;

			Version		version			= null;
			CultureInfo	culture			= null; // = new CultureInfo("en-US");
			byte[]		publicKeyToken		= null;
			byte[]		publicKey		= null;

			var		c			= stream.Peek();
			var		ch			= (char)c;

			while (ch == ',') {
				stream.Read();

				var keyword			= stream.nextIdentifier();
				if (keyword == null) {
					if (handleError == null) {
						throw new PrimInvalidOperandException("Expecting assembly property keyword, encountered end of input.");
					} else {
						handleError(null, "Expecting assembly property keyword, encountered end of input.");
					}
					return false;
				}
				if (keyword.Length < 1) {
					if (handleError == null) {
						throw new PrimInvalidOperandException("Assembly name property keyword must start with a letter.");
					} else {
						handleError(namePrefix, "Assembly name property keyword must start with a letter.");
					}
					return false;
				}
				if (!stream.nextMatches('=')) {
					if (handleError == null) {
						throw new PrimInvalidOperandException("Assembly name property keyword must be immediately followed by '='.");
					} else {
						handleError(namePrefix, "Assembly name property keyword must be immediately followed by '='.");
					}
					return false;
				}
				switch (keyword) {
					case "Version":
						if (version != null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName may only be specified once.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName may only be specified once.");
							}
							return false;
						}
						var major	= stream.nextUnsignedInteger();
						if (major == null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a major version number that is an unsigned integer.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a major version number that is an unsigned integer.");
							}
							return false;
						}
						if (!stream.nextMatches('.')) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a period between the major and minor version numbers.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a period between the major and minor version numbers.");
							}
							return false;
						}
						var minor	= stream.nextUnsignedInteger();
						if (major == null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a minor version number that is an unsigned integer.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a minor version number that is an unsigned integer.");
							}
							return false;
						}
						if (!stream.nextMatches('.')) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a period between the minor and build version numbers.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a period between the minor and build version numbers.");
							}
							return false;
						}
						var build	= stream.nextUnsignedInteger();
						if (major == null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a build version number that is an unsigned integer.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a build version number that is an unsigned integer.");
							}
							return false;
						}
						if (!stream.nextMatches('.')) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a period between the build and revision version numbers.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a period between the build and revision version numbers.");
							}
							return false;
						}
						var revision	= stream.nextUnsignedInteger();
						if (major == null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The version property of an AssemblyName requires a revision version number that is an unsigned integer.");
							} else {
								handleError(namePrefix, "The version property of an AssemblyName requires a revision version number that is an unsigned integer.");
							}
							return false;
						}
						version = new Version((int)major, (int)minor, (int)build, (int)revision);
						break;
					case "Culture":
						if (culture != null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The culture property of an AssemblyName may only be specified once.");
							} else {
								handleError(namePrefix, "The culture property of an AssemblyName may only be specified once.");
							}
							return false;
						}
						var cultureNameBuilder = new StringBuilder();
						var identifier = stream.nextIdentifier();
						if (String.IsNullOrEmpty(identifier)) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The culture property of an AssemblyName requires a language name in RFC-1766 format as its value. Specifically, it must begin with an identifier.");
							} else {
								handleError(namePrefix, "The culture property of an AssemblyName requires a language name in RFC-1766 format as its value. Specifically, it must begin with an identifier.");
							}
							return false;
						}
						cultureNameBuilder.Append(identifier);
						while (stream.nextMatches('-')) {
							identifier = stream.nextIdentifier();
							if (String.IsNullOrEmpty(identifier)) {
								if (handleError == null) {
									throw new PrimInvalidOperandException("The culture property of an AssemblyName requires a language name in RFC-1766 format as its value. Specifically, it cannot end with a hyphen.");
								} else {
									handleError(namePrefix, "The culture property of an AssemblyName requires a language name in RFC-1766 format as its value. Specifically, it cannot end with a hyphen.");
								}
								return false;
							}
							cultureNameBuilder.Append("-");
							cultureNameBuilder.Append(identifier);
						}
						var name = cultureNameBuilder.ToString();
						if (name == "neutral") {
							culture = CultureInfo.InvariantCulture;
						} else {
							culture = CultureInfo.GetCultureInfo(cultureNameBuilder.ToString());
						}
						break;
					case "PublicKey":
						if (publicKey != null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The public key/public key token property of an AssemblyName may only be specified once.");
							} else {
								handleError(namePrefix, "The public key/public key token property of an AssemblyName may only be specified once.");
							}
							return false;
						}
						var publicKeyBuilder		= parseBytesFromHexadecimal(stream);
						if (publicKeyBuilder == null) {
							var value = stream.nextIdentifier();
							if (!String.IsNullOrEmpty(value) && value == "null") {
								publicKey = nullPublicKeyOrKeyToken;
								break;
							}
							if (handleError == null) {
								throw new PrimInvalidOperandException("The public key property of an AssemblyName requires a hexadecimal number as its value.");
							} else {
								handleError(namePrefix, "The public key property of an AssemblyName requires a hexadecimal number as its value.");
							}
							return false;
						}
						if (publicKeyBuilder.Count % 2 != 0) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The public key property of an AssemblyName requires a hexadecimal number with an even number of digits as its value.");
							} else {
								handleError(namePrefix, "The public key property of an AssemblyName requires a hexadecimal number with an even number of digits as its value.");
							}
							return false;
						}
						publicKey = publicKeyBuilder.ToArray();
						break;
					case "PublicKeyToken":
						if (publicKeyToken != null) {
							if (handleError == null) {
								throw new PrimInvalidOperandException("The public key/public key token property of an AssemblyName may only be specified once.");
							} else {
								handleError(namePrefix, "The public key/public key token property of an AssemblyName may only be specified once.");
							}
							return false;
						}
						var publicKeyTokenBuilder		= parseBytesFromHexadecimal(stream);
						if (publicKeyTokenBuilder == null || publicKeyTokenBuilder.Count != 8) {
							var value = stream.nextIdentifier();
							if (!String.IsNullOrEmpty(value) && value == "null") {
								publicKeyToken = nullPublicKeyOrKeyToken;
								break;
							}
							if (handleError == null) {
								throw new PrimInvalidOperandException("The public key token property of an AssemblyName requires a 16-digit hexadecimal number as its value.");
							} else {
								handleError(namePrefix, "The public key token property of an AssemblyName requires a 16-digit hexadecimal number as its value.");
							}
							return false;
						}
						publicKeyToken = publicKeyTokenBuilder.ToArray(); 
						break;
					default:
						if (handleError == null) {
							throw new PrimInvalidOperandException("Unrecognized/unsupported assembly name property keyword: " + keyword + ".");
						} else {
							handleError(namePrefix, "Unrecognized/unsupported assembly name property keyword: " + keyword + ".");
						}
						return false;
				}

				c					= stream.Peek();
				ch					= (char)c;
			}

			if (version != null)				assemblyName.Version		= version;
			if (culture != null)				assemblyName.CultureInfo	= culture;

			if (publicKeyToken != null)			assemblyName.SetPublicKeyToken(publicKeyToken);
			if (publicKey != null)				assemblyName.SetPublicKey(publicKey);

			return true;

		}

		public static List<byte> parseBytesFromHexadecimal(TextReader stream) {
			byte byteValue				= 0;
			int nibbleIndex				= 0;

			var c					= stream.Peek();
			if (c < 0)				return null;
			var ch					= (char)c;
			var isValidDigit			= ch.isHexadecimalDigit();
			if (!isValidDigit)			return null;
			var bytes				= new List<byte>();
			while (isValidDigit) {
				var digit			= (char)stream.Read();
				if (nibbleIndex == 0) {
					byteValue		= (byte)digit.digitValue();
					nibbleIndex++;
				} else {
					byteValue		= (byte)((byte)(byteValue * 16) + (byte)digit.digitValue());
					bytes.Add(byteValue);
					byteValue	= 0;
					nibbleIndex	= 0;
				}

				c				= stream.Peek();
				if (c >= 0) { 
					ch				= (char)c;
					isValidDigit			= ch.isHexadecimalDigit();
				} else {
					isValidDigit			= false;
				}
			}
			if (nibbleIndex == 1) {
				bytes.Add((byte)(byteValue * 16));
			}
			return bytes;
		}

		public static TypeName fromString(String typeName) {
			// Leading whitespace is accepted. 
			// The namespace, any containing types and the assembly name may be omitted.
			// Generic types must at least specify their parameter arity, but may omit any type arguments.

			if (String.IsNullOrEmpty(typeName)) throw new PrimInvalidOperandException("Type name must not be empty or null.");
			return fromStream(new StringReader(typeName));
		}

		public static TypeName fromStream(TextReader stream) {
			// Leading whitespace is accepted.
			// The namespace, any containing types and the assembly name may be omitted.
			// Generic types must at least specify their parameter arity, but may omit any type arguments.
			// And don't blame me for the complexity of the code required to parse type name syntax. The following is what's required by what Microsoft hath wrought:
			
			List<String>	namespaceElements;
			List<String>	containingTypes;
			String		innerTypeElement;
			int		genericArity;
			TypeName[]	genericArguments;
			bool		isArray;

			parseQualifiedNameFromStream(stream, out namespaceElements, out containingTypes, out innerTypeElement, out genericArity, out genericArguments, out isArray);

			AssemblyName	assemblyName		= null;

			if (stream.nextMatches(',')) {
				var outerTypeElement = (containingTypes == null || containingTypes.Count < 1) ? innerTypeElement : containingTypes[0];
				if (!parseAssemblyName(
					stream, 
					out assemblyName, 
					(localPrefix, errorDescription) => {
						var prefix = namespaceElements.ToArray().compose(".");
						if (containingTypes != null) {
							prefix = prefix + outerTypeElement + ".";
							prefix = prefix + containingTypes.ToArray().compose("+");
						}
						throw new PrimInvalidOperandException(errorDescription + " TypeName context = " + prefix);
					})) {
					return null;
				}
			}

			if (genericArguments == null) {
				return isArray ?
					new TypeName(
						namespaceElements == null ? null : namespaceElements.ToArray(), 
						containingTypes == null ? null : containingTypes.ToArray(), 
						innerTypeElement, 
						true, 
						assemblyName) :
					new TypeName(
						namespaceElements == null ? null : namespaceElements.ToArray(), 
						containingTypes == null ? null : containingTypes.ToArray(), 
						innerTypeElement, 
						genericArity, 
						assemblyName);
			} else {
				return new TypeName(
						namespaceElements == null ? null : namespaceElements.ToArray(), 
						containingTypes == null ? null : containingTypes.ToArray(), 
						innerTypeElement, 
						genericArguments, 
						assemblyName);
			}

		}

		#endregion

		protected String[]	namespacePath;
		protected String[]	containingTypes;
		protected String	namePrefix;
		protected int		genericArity		= 0;
		protected TypeName[]	genericArguments;
		protected bool		isArray			= false;
		protected AssemblyName	assemblyName;

		protected Type		type;
		protected Assembly	assembly;

		public TypeName(String[] namespacePath, String name, AssemblyName assemblyName) 
			: this (namespacePath, null, name, false, assemblyName) {
		}

		public TypeName(String[] namespacePath, String[] containerPath, String name, AssemblyName assemblyName) 
			: this (namespacePath, containerPath, name, false, assemblyName) {
		}

		public TypeName(String[] namespacePath, String name, bool isArray, AssemblyName assemblyName) 
			: this (namespacePath, null, name, isArray, assemblyName) {
		}

		public TypeName(String[] namespacePath, String name, int genericArity, AssemblyName assemblyName) 
			: this (namespacePath, null, name, genericArity, assemblyName) {
		}

		public TypeName(String[] namespacePath, String name, TypeName[] genericArguments, AssemblyName assemblyName) 
			: this (namespacePath, null, name, genericArguments, assemblyName) {
		}

		public TypeName(String[] namespacePath, String[] containerPath, String namePrefix, bool isArray, AssemblyName assemblyName) {
			this.namespacePath			= namespacePath == null || namespacePath.Length < 1 ? null : namespacePath;
			this.containingTypes			= containerPath == null || containerPath.Length < 1 ? null : containerPath;
			this.namePrefix				= namePrefix;
			this.isArray				= isArray;
			this.assemblyName			= assemblyName;
		}

		public TypeName(String[] namespacePath, String[] containerPath, String namePrefix, int genericArity, AssemblyName assemblyName) {
			this.namespacePath			= namespacePath == null || namespacePath.Length < 1 ? null : namespacePath;
			this.containingTypes			= containerPath == null || containerPath.Length < 1 ? null : containerPath;
			this.namePrefix				= namePrefix;
			this.genericArity			= genericArity;
			this.assemblyName			= assemblyName;
		}

		public TypeName(String[] namespacePath, String[] containerPath, String namePrefix, TypeName[] genericArguments, AssemblyName assemblyName) {
			this.namespacePath			= namespacePath == null || namespacePath.Length < 1 ? null : namespacePath;
			this.containingTypes			= containerPath == null || containerPath.Length < 1 ? null : containerPath;
			this.namePrefix				= namePrefix;
			if (genericArguments == null || genericArguments.Length < 1) {
				this.genericArguments		= null;
				genericArity = 0;
			} else {
				this.genericArguments		= genericArguments;
				genericArity			= genericArguments.Length;
			}
			this.assemblyName			= assemblyName;
		}

		public TypeName(String qualifiedName, Assembly assembly) {
			List<String>	namespaceElements;
			List<String>	containingTypeList;

			parseQualifiedNameFromString(qualifiedName, out namespaceElements, out containingTypeList, out namePrefix, out genericArity, out genericArguments, out isArray);
			if (namespaceElements != null) namespacePath = namespaceElements.ToArray();
			if (containingTypeList != null) containingTypes = containingTypeList.ToArray();
			this.assembly = assembly;
			if (assembly != null) assemblyName = assembly.GetName();
		}

		public TypeName(Type type) {
			this.type = type;
			assembly = type.Assembly;
			assemblyName = type.Assembly.GetName();
			var nsResidentType = type;
			var containingType = type.DeclaringType;
			if (containingType != null) {
				var containers = new List<Type>();
				var parentType = containingType;
				while (parentType != null) {
					nsResidentType = parentType;
					containers.Add(parentType);
					parentType = parentType.DeclaringType;
				}
				containingTypes = new String[containers.Count];
				var limit = containingTypes.Length - 1;
				for (var i = limit; i >= 0; i--) containingTypes[limit - i] = containers[i].Name;
			}
			namespacePath = nsResidentType.Namespace.elementsFromString('.', null);
			if (type.IsGenericType) {
				namePrefix = new StringReader(type.Name).nextIdentifier();
				var myGenericArgs = type.GetGenericArguments();
				genericArity = myGenericArgs.Length;
				bool hasSameGenericTypeArgsAsContainer = false;
				if (containingType != null && containingType.IsGenericType) {
					var containersGenericArgs = containingType.GetGenericArguments();
					hasSameGenericTypeArgsAsContainer = containersGenericArgs.Length == genericArity;
				}
				if (hasSameGenericTypeArgsAsContainer) {
					genericArity = 0;
				} else if (!type.IsGenericTypeDefinition) {
					genericArguments = new TypeName[genericArity];
					for (var i = 0; i < genericArity; i++) {
						genericArguments[i] = new TypeName(myGenericArgs[i]);
					}
				}
			} else {
				isArray = type.IsArray;
				namePrefix = isArray ? type.GetElementType().Name : type.Name;
			}
		}

		public String NamePrefix {
			get {return namePrefix ?? "";}
		}

		public String Name {
			get {	var stream = new StringWriter();
				printNameOn(stream, false);
				return stream.ToString();}
		}

		public String NameWithModifyingSuffix {
			get {	var stream = new StringWriter();
				printNameOn(stream, true);
				return stream.ToString();}
		}

		public String FullName {
			get {	var stream = new StringWriter();
				printFullNameOn(stream, true);
				return stream.ToString();}
		}

		public String AssemblyQualifiedName {
			get {	var stream = new StringWriter();
				printOn(stream, true);
				return stream.ToString();}
		}

		public bool IsArray {
			get {return isArray;}
		}

		public int GenericArity {
			get {return genericArity;}
		}

		public AssemblyName AssemblyName {
			get {return assemblyName;}
		}

		public bool SpecifiesGenericType {
			get {return GenericArity > 0;}
		}

		public bool SpecifiesAssembly {
			get {return assemblyName != null;}
		}

		public bool SpecifiesGenericArguments {
			get {return genericArguments != null && genericArguments.Length > 0;}
		}

		public bool SpecifiesContaingType {
			get {return containingTypes != null && containingTypes.Length > 0;}
		}

		public bool SpecifiesNamespace {
			get {return namespacePath != null && namespacePath.Length > 0;}
		}

		public String GenericArgumentSuffix {
			get {	var stream = new StringWriter();
				if (SpecifiesGenericArguments) {
					stream.Write("[");
					printGenericArgumentsOn(stream);
					stream.Write("]");
				}
				return stream.ToString();}
		}

		public String ContainingTypesPath {
			get {	var stream = new StringWriter();
				printContainingTypesPathOn(stream);
				return stream.ToString();}
		}

		public String Namespace {
			get {	var stream = new StringWriter();
				printNamespaceOn(stream);
				return stream.ToString();}
		}

		public void genericArgumentsDo(System.Action<int, TypeName> enumerator2) {
			if (genericArguments == null) return;
			for (var i = 0; i < genericArguments.Length; i++) {
				var typeName = genericArguments[i];
				if (typeName != null) enumerator2(i, typeName);
			}
		}

		public void containingTypeNamesDo(System.Action<String> enumerator1) {
			if (containingTypes == null) return;
			for (var i = 0; i < containingTypes.Length; i++) {
				enumerator1(containingTypes[i]);
			}
		}

		public void namespacePathElementsDo(System.Action<String> enumerator1) {
			if (namespacePath == null) return;
			for (var i = 0; i < namespacePath.Length; i++) {
				enumerator1(namespacePath[i]);
			}
		}

		#region Printing (String Representation)

		public void printNamespaceOn(TextWriter stream) {
			if (namespacePath == null) return;
			var limit = namespacePath.Length - 1;
			for (var i = 0; i <= limit; i++) {
				var element = namespacePath[i];
				stream.Write(element);
				stream.Write(".");
			}
		}

		public void printContainingTypesPathOn(TextWriter stream) {
			if (containingTypes == null) return;
			var limit = containingTypes.Length - 1;
			for (var i = 0; i <= limit; i++) {
				var element = containingTypes[i];
				stream.Write(element);
				stream.Write("+");
			}
		}

		public void printGenericArgumentsOn(TextWriter stream) {
			if (genericArguments == null) return;
			var limit = genericArguments.Length - 1;
			for (var i = 0; i <= limit; i++) {
				var typeName = genericArguments[i];
				stream.Write("[");
				if (typeName == null) {
					stream.Write("T");
					stream.Write(i.ToString());
				} else {
					typeName.printOn(stream, false);
				}
				stream.Write("]");
				if (i < limit) stream.Write(",");
			}
		}

		public void printNameOn(TextWriter stream, bool includeGenericParametersOrArguments) {
			stream.Write(NamePrefix);
			if (GenericArity > 0) {
				stream.Write("`");
				stream.Write(GenericArity.ToString());
				if (includeGenericParametersOrArguments && genericArguments != null) {
					stream.Write("[");
					printGenericArgumentsOn(stream);
					stream.Write("]");
				}
			} else if (IsArray) {
				stream.Write("[]");
			}
		}

		public void printFullNameOn(TextWriter stream, bool includeGenericParametersOrArguments) {
			printNamespaceOn(stream);
			printContainingTypesPathOn(stream);
			printNameOn(stream, includeGenericParametersOrArguments);
		}

		public void printOn(TextWriter stream, bool includeGenericParametersOrArguments) {
			printFullNameOn(stream, includeGenericParametersOrArguments);
			if (SpecifiesAssembly) {
				stream.Write(", ");
				stream.Write(AssemblyName.FullName);
			}
		}

		public override String ToString() {
			var stream = new StringWriter();
			printOn(stream, true);
			return stream.ToString();
		}

		#endregion

		#region Resolving

		public Type Type {
			get {	return getType(true);}
		}

		public Assembly Assembly {
			get {	return getAssembly(true);}
		}

		public Type getType(bool raiseExceptionOnErrorOrNotFound) {
			return getType(null, raiseExceptionOnErrorOrNotFound);
		}

		public Type getType(Assembly[] candidateSourceAssemblies, bool raiseExceptionOnErrorOrNotFound) {
			if (type != null) return type;
			var name = FullName;
			var assembly = getAssembly(false);
			if (assembly == null) {
				if (candidateSourceAssemblies != null) {
					foreach (var sourceAssembly in candidateSourceAssemblies) {
						if (sourceAssembly != null) { 
							type = sourceAssembly.GetType(name, false);
							if (type != null) return type;
						}
					}
				}
				assembly = typeof(System.Object).Assembly;
				type = assembly.GetType(name, false);
				if (type != null) return type;
			} else { 
				return assembly.GetType(name, raiseExceptionOnErrorOrNotFound);
			}
			return System.Type.GetType(AssemblyQualifiedName, raiseExceptionOnErrorOrNotFound);
		}

		public Assembly getAssembly(bool raiseExceptionOnErrorOrNotFound) {
			if (assembly != null) return assembly;
			if (SpecifiesAssembly) {
				var currentAppDomain = AppDomain.CurrentDomain;
				if (raiseExceptionOnErrorOrNotFound) {
					assembly = currentAppDomain.Load(AssemblyName);
				} else {
					try {
						assembly = currentAppDomain.Load(AssemblyName);
					} catch {
					}
				}
				return assembly;
			} else  if (raiseExceptionOnErrorOrNotFound) {
				throw new AssemblyBindingFailure("No assembly specified.");
			} else {
				return null;
			}
		}

		#endregion

	}

}
