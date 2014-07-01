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
using System.Collections.Generic;
using Microsoft.Scripting;
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
using FuncNs = Microsoft.Scripting.Utils;
#else
using System.Linq.Expressions;
using Expression =System.Linq.Expressions.Expression;
using FuncNs = System;
#endif
using EssenceSharp.ParsingServices;
using EssenceSharp.Runtime;
using EssenceSharp.Runtime.Binding;
#endregion

namespace EssenceSharp.CompilationServices {

	public class ESCompiler {

		#region Instance Variables

		protected ESObjectSpace								objectSpace						= null;
		protected ESParser								parser						= null;
		protected ASTGenerator								abstractSyntaxTreeGenerator			= null;

		protected int									errorCount					= 0;
		protected System.Action<String, SourceSpan, int, Severity> 			reportError 					= null;
		protected System.Action<String, SourceSpan>					parameterNameCollisionAction			= null;
		protected System.Action<String, SourceSpan>					variableNameCollisionAction			= null;
		protected System.Action<NamedValueOccurrence, SourceSpan>			illegalAssignmentAction				= null;
		protected System.Action<String, SourceSpan>					primitiveSpeficationErrorAction			= null;
		protected System.Action<System.Collections.Generic.HashSet<ESSymbol>, SourceSpan> undeclaredVariableReferencesAction = null;
		
		#endregion

		#region Constructors

		public ESCompiler(ESObjectSpace objectSpace, TextReader sourceStream) 
			: this (objectSpace, sourceStream, SyntaxProfile.Universal) {
		}
		
		public ESCompiler(ESObjectSpace objectSpace, TextReader sourceStream, SyntaxProfile syntaxProfile) 
			: this (objectSpace, new ESParser(sourceStream, syntaxProfile)) {
		}
		
		public ESCompiler(ESObjectSpace objectSpace, TextReader sourceStream, ParsingOptions parsingOptions) 
			: this (objectSpace, new ESParser(sourceStream, parsingOptions)) {
		}

		public ESCompiler(ESObjectSpace objectSpace, ESParser parser) {
			this.objectSpace = objectSpace;
			Parser = parser;
		}

		protected virtual void bindToParser() {
			if (parser == null) return;
			if (reportError == null) {
				reportError = parser.ReportError;
			} else {
				parser.ReportError = reportError;
			}
		}

		#endregion

		#region Factory methods

		protected virtual ASTGenerator newASTGenerator() {
			return new ASTGenerator(this, newCodeGenerationContext());
		}

		protected virtual ASTGenerator newASTGenerator(Object selfValue, List<ParameterExpression> rootParameters) {
			return new ASTGenerator(this, newCodeGenerationContext(selfValue, rootParameters));
		}

		public virtual CodeGenerationContext newCodeGenerationContext() {
			return new CodeGenerationContext(this);
		}

		public virtual CodeGenerationContext newCodeGenerationContext(Object selfValue, List<ParameterExpression> rootParameters) {
			return new CodeGenerationContext(this, selfValue, rootParameters == null || rootParameters.Count < 1 ? null : rootParameters.ToArray());
		}

		public List<ParameterExpression> parametersFor(Object[] arguments) {
			if (arguments == null || arguments.Length < 1) return null;
			List<ParameterExpression> rootParameters = new List<ParameterExpression>();
			for (var i = 0; i < arguments.Length; i++) {
				rootParameters.Add(Expression.Parameter(TypeGuru.objectType, "_a" + (i + 1)));
			}	
			return rootParameters;
		}

		#endregion

		#region Public Protocol

		public enum ScriptType {
			BlockDeclaration,
			SelfExpression
		}

		#region Properties

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		public ESParser Parser {
			get {return parser;}
			set {if (value == parser) return;
				parser = value;
				bindToParser();}
		}
		
		public int ErrorCount {
			get {return errorCount + Parser.ErrorCount;}
		}

		public System.Action<String, SourceSpan, int, Severity> ReportError {
			get {return reportError;}
			set {	reportError = value;
				bindToParser();}
		}	

		public System.Action<String, SourceSpan> ParameterNameCollisionAction {
			set {parameterNameCollisionAction = value;}
		}

		public System.Action<String, SourceSpan> VariableNameCollisionAction {
			set {variableNameCollisionAction = value;}
		}

		public System.Action<NamedValueOccurrence, SourceSpan> IllegalAssignmentAction {
			set {illegalAssignmentAction = value;}
		}

		public System.Action<String, SourceSpan> PrimitiveSpeficationErrorAction {
			set {primitiveSpeficationErrorAction = value;}
		}
 
		public System.Action<System.Collections.Generic.HashSet<ESSymbol>, SourceSpan> UndeclaredVariableReferencesAction {
			set {undeclaredVariableReferencesAction = value;}
		}

		#endregion

		public virtual bool evaluate(NamespaceObject environment, Object[] arguments, out Object value) {
			return evaluate(environment, null, arguments, out value);
		}

		public virtual bool evaluate(NamespaceObject environment, Object selfValue, Object[] arguments, out Object value) {
			return evaluate(ScriptType.BlockDeclaration, environment, selfValue, arguments, out value);
		}

		public virtual bool evaluateSelfExpression(NamespaceObject environment, Object selfValue, Object[] arguments, out Object value) {
			return evaluate(ScriptType.SelfExpression, environment, selfValue, arguments, out value);
		}

		public virtual bool evaluate(ScriptType rootParseNodeType, NamespaceObject environment, Object selfValue, Object[] arguments, out Object value) {
			ESBlock compiledBlock;
			if (compile(rootParseNodeType, environment, selfValue, rootParseNodeType == ScriptType.SelfExpression ? parametersFor(arguments) : null, out compiledBlock)) {
				value = arguments == null || arguments.Length < 1 ? compiledBlock.value0() : compiledBlock.valueWithArguments(arguments);
				return true;
			}

			value = null;
			return false;
		}

		public virtual bool compile(NamespaceObject environment, out ESBlock block) {
			return compile(ScriptType.BlockDeclaration, environment, null, null, out block);
		}

		public virtual bool compile(NamespaceObject environment, Object selfValue, out ESBlock block) {
			return compile(ScriptType.BlockDeclaration, environment, selfValue, null, out block);
		}

		public virtual bool compileSelfExpression(NamespaceObject environment, Object selfValue, List<ParameterExpression> rootParameters, out ESBlock block) {
			return compile(ScriptType.SelfExpression, environment, selfValue, rootParameters, out block);
		}
		
		public virtual bool compile(ScriptType rootParseNodeType, NamespaceObject environment, Object selfValue, List<ParameterExpression> rootParameters, out ESBlock block) {
			Expression<FuncNs.Func<Object>> lambda;
			if (compile(rootParseNodeType, environment, selfValue, rootParameters, out lambda)) {
				var function = lambda.Compile();
				if (function == null) {
					block = null;
					return false;
				}
				block = (ESBlock)function();
				return true;
			}

			block = null;
			return false;
		}

		public virtual bool compileMethod(BehavioralObject methodClass, ESSymbol protocol, out ESMethod compiledMethod) {

			ESMethod method = null;
			if (compileMethod( 
				methodClass, 
				(MethodDeclarationNode methodDeclarationNode, Delegate function) => {
					if (function == null) return false;
					method = objectSpace.newMethod(methodClass, methodClass, methodDeclarationNode, protocol);
					return true;
				})) {
				compiledMethod = method;
				return true;
			} else {
				compiledMethod = null;
				return false;
			}

		}

		public CodeGenerationContext Context {
			get {return abstractSyntaxTreeGenerator.Context;}
		}

		#endregion

		#region Partial Compilations

		public virtual bool compile(ScriptType rootParseNodeType, Object selfValue, List<ParameterExpression> rootParameters, out BlockLiteralNode blockLiteralNode) {

			ParseTreeNode rootParseNode;
			List<String> parameterNames = null;
			if (rootParameters != null) {
				parameterNames = new List<String>();
				foreach (var parameter in rootParameters) parameterNames.Add(parameter.Name);
			}
			switch (rootParseNodeType) {
				default:
				case ScriptType.BlockDeclaration:
					rootParseNode = parser.parseTree();
					break;
				case ScriptType.SelfExpression:
					rootParseNode = parser.selfExpressionParseTree(parameterNames);
					break;
			}

			if (parser.ErrorCount > 0 || !rootParseNode.IsBlockLiteral) {
				blockLiteralNode = null;
				return false;
			}

			abstractSyntaxTreeGenerator = newASTGenerator(selfValue, rootParameters);
			blockLiteralNode = abstractSyntaxTreeGenerator.applyTo(rootParseNode) as BlockLiteralNode;
			if (blockLiteralNode == null) return false;
			if (ErrorCount > 0) return false;
			return blockLiteralNode.IsCompilable;

		}

		public virtual bool compile(ScriptType rootParseNodeType, NamespaceObject environment, Object selfValue, List<ParameterExpression> rootParameters, out Expression expression) {
			BlockLiteralNode blockLiteralNode;
			if (compile(rootParseNodeType, selfValue, rootParameters, out blockLiteralNode)) {
				expression = blockLiteralNode.asCLRExpression(environment, null);
				var undeclaredVariables = blockLiteralNode.UndeclaredVariables;
				if (undeclaredVariables != null && undeclaredVariables.Count > 0)
					handleUndeclaredVariableReferences(undeclaredVariables, SourceSpan.None);
				return ErrorCount < 1;
			}

			expression = null;
			return false;
		}

		public virtual bool compile(ScriptType rootParseNodeType, NamespaceObject environment, Object selfValue, List<ParameterExpression> rootParameters, out Expression<FuncNs.Func<Object>> lambda) {

			Expression rootExpression;
			if (compile(rootParseNodeType, environment, selfValue, rootParameters, out rootExpression)) {
				lambda = Expression.Lambda<FuncNs.Func<Object>>(rootExpression, false, new ParameterExpression[0]);
				return true;
			}

			lambda = null;
			return false;

		}

		public virtual bool compileMethod(BehavioralObject methodClass, Functor1<bool, MethodDeclarationNode> compileMethodFromMethodDeclarationNode) {

			ParseTreeNode rootParseNode = parser.methodParseTree();
			if (parser.ErrorCount > 0) return false;
			switch (rootParseNode.ParseNodeType) {
				case ParseNodeType.MethodDeclaration:
				case ParseNodeType.PrimitiveMethodDeclaration:
					abstractSyntaxTreeGenerator = newASTGenerator();
					var methodDeclarationNode = abstractSyntaxTreeGenerator.applyTo(rootParseNode) as MethodDeclarationNode; 
					if (methodDeclarationNode == null) return false;
					if (ErrorCount < 1 && !methodDeclarationNode.IsCompilable) return false;
					return compileMethodFromMethodDeclarationNode(methodDeclarationNode);
				default:
					return false;
			}

		}

		public virtual bool compileMethod(BehavioralObject methodClass, Functor2<bool, MethodDeclarationNode, LambdaExpression> compileMethodFromLambdaExpression) {
			return compileMethod(methodClass, (MethodDeclarationNode methodDeclarationNode) => {
								var lambda = methodDeclarationNode.lambdaFor(methodClass, methodClass);
								var undeclaredVariables = methodDeclarationNode.UndeclaredVariables;
								if (undeclaredVariables != null && undeclaredVariables.Count > 0)
									handleUndeclaredVariableReferences(undeclaredVariables, SourceSpan.None);
								if (ErrorCount > 0) return false;
								return compileMethodFromLambdaExpression(methodDeclarationNode, lambda);});
		}

		public virtual bool compileMethod(BehavioralObject methodClass, Functor2<bool, MethodDeclarationNode, Delegate> createCompiledMethodFromDelegate) {
			return compileMethod(methodClass, (MethodDeclarationNode methodDeclarationNode, LambdaExpression lambdaExpression) => {
								return createCompiledMethodFromDelegate(methodDeclarationNode, lambdaExpression.Compile());});
		}

		#endregion

		#region Error Handling

		public virtual void handleParameterNameCollision(String parameterName, SourceSpan span) {
			errorCount++;
			if (parameterNameCollisionAction != null) {
				parameterNameCollisionAction(parameterName, span);
				return;
			}
			StringBuilder sb = new StringBuilder();
			sb.Append("Parameter name collision: ");
			sb.Append(parameterName);
			ReportError(sb.ToString(), span, 0, Severity.Error);
		}

		public virtual void handleVariableNameCollision(String variableName, SourceSpan span) {
			errorCount++;
			if (variableNameCollisionAction != null) {
				variableNameCollisionAction(variableName, span);
				return;
			}
			StringBuilder sb = new StringBuilder();
			sb.Append("Variable name collision: ");
			sb.Append(variableName);
			ReportError(sb.ToString(), span, 0, Severity.Error);
		}

		public virtual void handleIllegalAssignment(NamedValueOccurrence unassignableIdentifier, SourceSpan span) {
			errorCount++;
			if (illegalAssignmentAction != null) {
				illegalAssignmentAction(unassignableIdentifier, span);
				return;
			}
			StringBuilder sb = new StringBuilder();
			sb.Append("Attempt to assign a value to a constant: ");
			sb.Append(unassignableIdentifier.NameString);
			ReportError(sb.ToString(), span, 0, Severity.Error);
		}

		public virtual void handlePrimitiveSpeficationError(String errorMessage, SourceSpan span) {
			errorCount++;
			if (primitiveSpeficationErrorAction != null) {
				primitiveSpeficationErrorAction(errorMessage, span);
				return;
			}
			StringBuilder sb = new StringBuilder();
			sb.Append("Primitive specification error: ");
			sb.Append(errorMessage);
			ReportError(sb.ToString(), span, 0, Severity.Error);
		}

		public virtual void handleUndeclaredVariableReferences(System.Collections.Generic.HashSet<ESSymbol> undeclaredVariables, SourceSpan span) {
			errorCount++;
			if (undeclaredVariableReferencesAction != null) {
				undeclaredVariableReferencesAction(undeclaredVariables, span);
				return;
			}
			StringBuilder sb = new StringBuilder();
			sb.AppendLine("Undeclared variable reference(s): ");
			foreach (var varName in undeclaredVariables) {
				sb.Append("\t");
				sb.AppendLine(varName.PrimitiveValue);
			}
			ReportError(sb.ToString(), span, 0, Severity.Error);
		}

		#endregion

	}

}
