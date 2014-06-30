// Copyright 2014 Quartz Technologies, Ltd. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Quartz Technologies Ltd. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Rename;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Utilities;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Rename.ConflictEngine;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Simplification;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace Quartz.CodeAnalysis.Asyncable
{
    internal static class CodeGeneration
    {
        private const string AsyncMethodDefaultFormat = "{0}Async";
        private const string ThreadingCancellationTokenStructName = "global::System.Threading.CancellationToken";
        private const string ThreadingCancellationTokenVariableName = "cancellationToken";
        private const string TaskClassName = "global::System.Threading.Tasks.Task";
        private const string GenericTaskClassName = "global::System.Threading.Tasks.Task<{0}>";

        /// <summary>
        /// Produce an async method signature variant of the given sync method.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The sync method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The produced async method signature (and only signature)</returns>
        public static Task<MethodDeclarationSyntax> ImplementAsyncMethodVariantSignature(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            /** Name **/
            string asyncMethodName = string.Format(AsyncMethodDefaultFormat, methodDecl.Identifier.ValueText);

            /** Return Type **/
            var asyncMethodReturnType = ConvertToAsyncType(document, methodDecl.ReturnType, semanticModel, cancellationToken);

            /** Parameters **/
            var cancellationTokenType = SyntaxFactory.ParseTypeName(ThreadingCancellationTokenStructName);

            var asyncMethodParamsList = SyntaxFactory.ParameterList().WithParameters(methodDecl.ParameterList.Parameters);
            asyncMethodParamsList = asyncMethodParamsList.AddParameters(CreateADefaultCancellationTokenParameter());

            //-------------------------------------------------------
            //  Create the async method declaration
            //-------------------------------------------------------

            var asyncMethodDecl = SyntaxFactory.MethodDeclaration(
                attributeLists: methodDecl.AttributeLists,
                modifiers: methodDecl.Modifiers,
                returnType: asyncMethodReturnType, // methodDecl.ReturnType,
                explicitInterfaceSpecifier: methodDecl.ExplicitInterfaceSpecifier,
                identifier: SyntaxFactory.Identifier(asyncMethodName),
                typeParameterList: methodDecl.TypeParameterList,
                parameterList: asyncMethodParamsList,
                constraintClauses: methodDecl.ConstraintClauses,
                body: null,
                semicolonToken: SyntaxFactory.Token(SyntaxKind.SemicolonToken)/*hasNoBody ? SyntaxFactory.Token(SyntaxKind.SemicolonToken) : new SyntaxToken()*/
                );

            //-------------------------------------------------------
            //  Alter the documentation to match the changes
            //-------------------------------------------------------

            var newTrivia = (AddParameterToDocumentationTrivia(
                document: document,
                trivia: asyncMethodReturnType.GetLeadingTrivia(),
                parameterName: ThreadingCancellationTokenVariableName,
                parameterDescription: "A cancellation token that can be used to cancel the operation.",
                semanticModel: semanticModel,
                cancellationToken: cancellationToken
                ));

            asyncMethodDecl = asyncMethodDecl.WithReturnType(asyncMethodReturnType.WithLeadingTrivia(newTrivia));

            /* Done. */
            return Task.FromResult(
                asyncMethodDecl.WithAdditionalAnnotations(Formatter.Annotation)
                );
        }

        /// <summary>
        /// Produce an async method signature variant of the given sync method.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The async method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The produced async method signature (and only signature)</returns>
        public static Task<MethodDeclarationSyntax> ImplementSyncMethodVariantSignature(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            /** Name **/
            string syncMethodName = Regex.Replace(methodDecl.Identifier.ValueText, "(Async|TaskAsync)$", "", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);

            /** Return Type **/
            var syncMethodReturnType = ConvertToSyncType(document, methodDecl.ReturnType, semanticModel, cancellationToken);

            /** Parameters **/
            var syncMethodParams = methodDecl.ParameterList;

            /** Gets the initial documentation trivia **/
            var documentationTrivia = syncMethodReturnType.GetLeadingTrivia();

            //-------------------------------------------------------
            //  Remove the cancellation token from the parameter list
            //-------------------------------------------------------

            /* Note: Yes, in 99% of cases, we won't have 2 ore more cancellation tokens, but I do want to cover these absurd cases.
               Note (2): I'd rather do a comprasion without hand-coding the type, but unfortunatly, I can't execute sematicModel.GetTypeInfo() on any type that don't exists in the model. */
            foreach (var param in syncMethodParams.Parameters)
            {
                var typeInfo = semanticModel.GetTypeInfo(param.Type);
                if (typeInfo.ConvertedType.Name == "CancellationToken" && typeInfo.ConvertedType.ContainingNamespace.ToDisplayString() == "System.Threading")
                {
                    /*  Alter the documentation and removes it */
                    documentationTrivia = RemoveParameterFromDocumentationTrivia(
                        document: document,
                        trivia: documentationTrivia,
                        parameterName: param.Identifier.ValueText,
                        semanticModel: semanticModel,
                        cancellationToken: cancellationToken
                    );

                    /* Remove it */
                    syncMethodParams = syncMethodParams.RemoveNode(param, SyntaxRemoveOptions.KeepNoTrivia);
                }
            }

            syncMethodReturnType = syncMethodReturnType.WithLeadingTrivia(documentationTrivia);

            //-------------------------------------------------------
            //  Create the async method declaration
            //-------------------------------------------------------

            var syncMethodDecl = SyntaxFactory.MethodDeclaration(
                attributeLists: methodDecl.AttributeLists,
                modifiers: methodDecl.Modifiers,
                returnType: syncMethodReturnType,
                explicitInterfaceSpecifier: methodDecl.ExplicitInterfaceSpecifier,
                identifier: SyntaxFactory.Identifier(syncMethodName),
                typeParameterList: methodDecl.TypeParameterList,
                parameterList: syncMethodParams,
                constraintClauses: methodDecl.ConstraintClauses,
                body: null,
                semicolonToken: SyntaxFactory.Token(SyntaxKind.SemicolonToken)
                );

            return Task.FromResult(
                syncMethodDecl.WithAdditionalAnnotations(Formatter.Annotation)
                );
        }

        /// <summary>
        /// Produce an async method signature and basic implementation variant of the given sync method.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The sync method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The produced async method.</returns>
        public static async Task<MethodDeclarationSyntax> ImplementAsyncMethodVariant(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            /** Generate the method signature **/
            var signature = (await ImplementAsyncMethodVariantSignature(document, methodDecl, semanticModel, cancellationToken))
                .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.None));

            //-------------------------------------------------------
            //  In order to create an async method variant, we should use the Task.Factory.StartNew method.
            //-------------------------------------------------------

            MemberAccessExpressionSyntax factoryAccess;
            ParenthesizedLambdaExpressionSyntax taskExecutionLambda;
            ExpressionSyntax syncMethodAccessExpression;

            /* Build the sync method invocation statement (e.g. FooAsync should Foo(a, b)). This statement will be used inside the Task.Factory.StartNew lambda. */
            //  If we're dealing with static method, we shouldn't use "this", otherwise, its good to specify it and give Roslyn simplifier to figure if we really needs it.
            if (methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword)))
            {
                syncMethodAccessExpression = SyntaxFactory.IdentifierName(methodDecl.Identifier.ValueText);
            }
            else
            {
                syncMethodAccessExpression = SyntaxFactory.MemberAccessExpression(
                        kind: SyntaxKind.SimpleMemberAccessExpression,
                        expression: SyntaxFactory.ThisExpression(),
                        name: SyntaxFactory.IdentifierName(methodDecl.Identifier.ValueText)
                    ).WithAdditionalAnnotations(Simplifier.Annotation);
            }

            //  Build the sync method invocation expression
            var syncMethodInvocationExpression = SyntaxFactory.InvocationExpression(
                expression: syncMethodAccessExpression,
                argumentList: SyntaxFactory.ArgumentList(
                    arguments: SyntaxFactory.SeparatedList((from p in methodDecl.ParameterList.Parameters
                                                            select SyntaxFactory.Argument(SyntaxFactory.IdentifierName(name: p.Identifier.ValueText))))
                    )
                );


            /* Use the right Task type (simple Task or Generic Task<T>) */
            if (methodDecl.ReturnType is PredefinedTypeSyntax
                && ((PredefinedTypeSyntax)methodDecl.ReturnType).Keyword.IsKind(SyntaxKind.VoidKeyword))
            {
                factoryAccess = SyntaxFactory.MemberAccessExpression(
                    kind: SyntaxKind.SimpleMemberAccessExpression,
                    expression: SyntaxFactory.ParseTypeName(TaskClassName).WithAdditionalAnnotations(Simplifier.Annotation),
                    name: SyntaxFactory.IdentifierName("Factory")
                );
                
                taskExecutionLambda = SyntaxFactory.ParenthesizedLambdaExpression(SyntaxFactory.Block(
                   SyntaxFactory.ExpressionStatement(syncMethodInvocationExpression)
                ));
            }
            else
            {
                factoryAccess = SyntaxFactory.MemberAccessExpression(
                    kind: SyntaxKind.SimpleMemberAccessExpression,
                    expression: SyntaxFactory.ParseTypeName(string.Format(GenericTaskClassName, methodDecl.ReturnType.ToFullString())).WithAdditionalAnnotations(Simplifier.Annotation),
                    name: SyntaxFactory.IdentifierName("Factory")
                );

                taskExecutionLambda = SyntaxFactory.ParenthesizedLambdaExpression(SyntaxFactory.Block(
                    SyntaxFactory.ReturnStatement(syncMethodInvocationExpression)
                ));
            }

            //-------------------------------------------------------
            //  Build the actual body
            //-------------------------------------------------------

            signature = signature.WithBody(SyntaxFactory.Block(SyntaxFactory.ReturnStatement(
                SyntaxFactory.InvocationExpression(
                    expression: SyntaxFactory.MemberAccessExpression(
                        kind: SyntaxKind.SimpleMemberAccessExpression,
                        expression: factoryAccess, // (Task|Task<T>).Factory
                        name: SyntaxFactory.IdentifierName("StartNew")
                        ),
                    argumentList: SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(new[] {
                            /* StartNew first argument - the lambda callback */
                            SyntaxFactory.Argument(taskExecutionLambda),

                            /* StartNew optional argument - cancellationt token */
                            SyntaxFactory.Argument(
                                nameColon: SyntaxFactory.NameColon(SyntaxFactory.IdentifierName(ThreadingCancellationTokenVariableName), SyntaxFactory.Token(SyntaxKind.ColonToken)),
                                refOrOutKeyword: SyntaxFactory.Token(SyntaxKind.None),
                                expression: SyntaxFactory.IdentifierName(ThreadingCancellationTokenVariableName)
                            )
                        })
                    )
                )
            )));

            //-------------------------------------------------------
            //  Done
            //-------------------------------------------------------

            return signature;
        }

        /// <summary>
        /// Produce a sync method signature and basic implementation variant of the given async method.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The async method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The produced sync method.</returns>
        public static async Task<MethodDeclarationSyntax> ImplementSyncMethodVariant(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            /** Generate the method signature **/
            var signature = (await ImplementSyncMethodVariantSignature(document, methodDecl, semanticModel, cancellationToken))
                .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.None));

            //-------------------------------------------------------
            //  In order to trigger the async method, we should build the argument list.
            //  It should have been a simple task. However, since we can't know if the async method requests a cancellation token,
            //  and if it does, we can't know if this cancellation token is optional or not, we must cover this case and send default(CancellationToken) in case of need.
            //-------------------------------------------------------

            var argumentsList = new List<ArgumentSyntax>();
            bool shouldAssignNamedParamter = false;
            foreach (var param in methodDecl.ParameterList.Parameters)
            {
                var typeInfo = semanticModel.GetTypeInfo(param.Type);
                if (typeInfo.ConvertedType.Name == "CancellationToken" && typeInfo.ConvertedType.ContainingNamespace.ToDisplayString() == "System.Threading")
                {
                    // This is a required argument?
                    if (param.Default != null)
                    {
                        argumentsList.Add(SyntaxFactory.Argument(SyntaxFactory.IdentifierName(name: param.Identifier.ValueText)));
                    }
                    else
                    {
                        shouldAssignNamedParamter = true; // From now on, require to send optional argument using named parameters.

                        argumentsList.Add(SyntaxFactory.Argument(
                                nameColon: SyntaxFactory.NameColon(SyntaxFactory.IdentifierName(param.Identifier.ValueText), SyntaxFactory.Token(SyntaxKind.ColonToken)),
                                refOrOutKeyword: SyntaxFactory.Token(SyntaxKind.None),
                                expression: SyntaxFactory.DefaultExpression(SyntaxFactory.ParseTypeName(ThreadingCancellationTokenStructName).WithAdditionalAnnotations(Simplifier.Annotation))
                            ));
                    }
                }
                else
                {
                    // We're dealing with any parameter other than cancellation token - so we can just add it.
                    if (param.Default == null)
                    {
                        // Default argument - just add it.
                         argumentsList.Add(SyntaxFactory.Argument(SyntaxFactory.IdentifierName(name: param.Identifier.ValueText)));
                    }
                    else
                    {
                        // This is an optional parameter. We got two cases here:
                        //      1. If this is the last parameter, we can just ignore it.
                        //      2. Otherwise, we must set a flag that requires us to continue by providing named arguments.
                        //
                        //      Examples:
                        //          void FooAsync(int a, int b = 2, CancellationToken cancellationToken = default(CancellationToken))
                        //              In this case, we can just ignore the argument.
                        //      
                        //          void FooAsync(int a, int b = 2, CancellationToken cancellationToken = default(CancellationToken), int c = 3)
                        //              In this case, we can must specify "c" as named parameter - FooAsync(a, b, c: c). Otherwise, we'll break the method call.

                        if (shouldAssignNamedParamter)
                        {
                            argumentsList.Add(SyntaxFactory.Argument(
                               nameColon: SyntaxFactory.NameColon(SyntaxFactory.IdentifierName(param.Identifier.ValueText), SyntaxFactory.Token(SyntaxKind.ColonToken)),
                                refOrOutKeyword: SyntaxFactory.Token(SyntaxKind.None),
                                expression: SyntaxFactory.IdentifierName(param.Identifier.ValueText)
                                ));
                        }
                        else
                        {
                            argumentsList.Add(SyntaxFactory.Argument(SyntaxFactory.IdentifierName(name: param.Identifier.ValueText)));
                        }
                    }
                }
            }

            //-------------------------------------------------------
            //  Add the async method call expression
            //-------------------------------------------------------

            ExpressionSyntax asyncMethodAccessorExpression;
            InvocationExpressionSyntax asyncMethodInvocationExpression;

            //  If we're dealing with static method, we shouldn't use "this", otherwise, its good to specify it and give Roslyn simplifier to figure if we really needs it.
            if (methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword)))
            {
                asyncMethodAccessorExpression = SyntaxFactory.IdentifierName(methodDecl.Identifier.ValueText);
            }
            else
            {
                asyncMethodAccessorExpression = SyntaxFactory.MemberAccessExpression(
                        kind: SyntaxKind.SimpleMemberAccessExpression,
                        expression: SyntaxFactory.ThisExpression(),
                        name: SyntaxFactory.IdentifierName(methodDecl.Identifier.ValueText)
                    ).WithAdditionalAnnotations(Simplifier.Annotation);
            }

            asyncMethodInvocationExpression = SyntaxFactory.InvocationExpression(
                expression: asyncMethodAccessorExpression,
                argumentList: SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(argumentsList))
                );


            //-------------------------------------------------------
            //  Buiid the actual body
            //-------------------------------------------------------

            var asyncInvocationExpression = SyntaxFactory.InvocationExpression(
                    expression: SyntaxFactory.MemberAccessExpression(
                        kind: SyntaxKind.SimpleMemberAccessExpression,
                        expression: SyntaxFactory.InvocationExpression(
                            expression: SyntaxFactory.MemberAccessExpression(
                                kind: SyntaxKind.SimpleMemberAccessExpression,
                                expression: asyncMethodInvocationExpression,
                                name: SyntaxFactory.IdentifierName("GetAwaiter")
                        )),
                        name: SyntaxFactory.IdentifierName("GetResult")
                    ));

            /* Use the right Task type (simple Task or Generic Task<T>) */
            if (methodDecl.ReturnType is PredefinedTypeSyntax
                && ((PredefinedTypeSyntax)methodDecl.ReturnType).Keyword.IsKind(SyntaxKind.VoidKeyword))
            {
                signature = signature.WithBody(SyntaxFactory.Block(SyntaxFactory.ExpressionStatement(asyncInvocationExpression)));
            }
            else
            {
                signature = signature.WithBody(SyntaxFactory.Block(SyntaxFactory.ReturnStatement(asyncInvocationExpression)));
            }

            //-------------------------------------------------------
            //  Since we're dealing with async method, we should 
            //  remove the "async" reserved keyword if it was assigned
            //-------------------------------------------------------

            var asyncKeywordModifier = signature.Modifiers.FirstOrDefault(m => m.IsKind(SyntaxKind.AsyncKeyword));
            if (!asyncKeywordModifier.IsKind(SyntaxKind.None))
            {
                signature = signature.WithModifiers(signature.Modifiers.Remove(asyncKeywordModifier));
            }

            //-------------------------------------------------------
            //  Done
            //-------------------------------------------------------

            return signature;

        }

        /// <summary>
        /// Adds a using statement to the given compilation unit, if it's not already present.
        /// </summary>
        /// <param name="root">The compilation unit root node.</param>
        /// <param name="name">The using statement name.</param>
        /// <returns>The compilation unit with the stated using statement.</returns>
        private static CompilationUnitSyntax WithUsing(this CompilationUnitSyntax root, string name)
        {
            if (!root.Usings.Any(u => u.Name.ToString() == name))
            {
                root = root.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(name)).WithAdditionalAnnotations(Formatter.Annotation));
            }

            return root;
        }

        /// <summary>
        /// Adds a carridge return to the end of the given trivia list.
        /// </summary>
        /// <param name="currentTrivia">The current trivia.</param>
        /// <returns>The modified trivia list.</returns>
        public static SyntaxTriviaList WithCarriageReturn(this SyntaxTriviaList currentTrivia)
        {
            currentTrivia = currentTrivia.Insert(0, SyntaxFactory.CarriageReturnLineFeed);
            return currentTrivia;
        }

        #region Helpers

        /// <summary>
        /// Creates a new, default initialized, optional, <see cref="ParameterSyntax"/> for <see cref="CancellationToken"/>.
        /// </summary>
        /// <returns>The created parameter.</returns>
        private static ParameterSyntax CreateADefaultCancellationTokenParameter()
        {
            var cancellationTokenType = SyntaxFactory.ParseTypeName(ThreadingCancellationTokenStructName);

            return SyntaxFactory.Parameter(
                attributeLists: default(SyntaxList<AttributeListSyntax>),
                modifiers: default(SyntaxTokenList),
                identifier: SyntaxFactory.Identifier(SyntaxFactory.TriviaList(SyntaxFactory.ElasticSpace), ThreadingCancellationTokenVariableName, default(SyntaxTriviaList)),
                type: cancellationTokenType,
                @default: SyntaxFactory.EqualsValueClause(
                    equalsToken: SyntaxFactory.Token(
                        leading: SyntaxFactory.TriviaList(SyntaxFactory.ElasticSpace),
                        kind: SyntaxKind.EqualsToken,
                        trailing: SyntaxFactory.TriviaList(SyntaxFactory.ElasticSpace)
                        ),
                    value: SyntaxFactory.DefaultExpression(cancellationTokenType)
                    )
                ).WithAdditionalAnnotations(Simplifier.Annotation);
        }

        /// <summary>
        /// Converts the given type into a task representation
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="underlayngType">The underlaying type.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The converted type.</returns>
        private static TypeSyntax ConvertToAsyncType(Document document, TypeSyntax underlayngType, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------
            TypeSyntax wrapperType;

            //  The underlaying type holds the method trailing trivia (which is typicly the method documentation)
            //  So lets transfer it.
            var leadingTrivia = SyntaxTriviaList.Empty;
            var trailingTrivia = SyntaxTriviaList.Empty;

            /** Leading **/
            if (underlayngType.HasLeadingTrivia)
            {
                //  Save it
                leadingTrivia = underlayngType.GetLeadingTrivia();

                //  Remove it from the underlaying type
                underlayngType = underlayngType.WithLeadingTrivia(SyntaxTriviaList.Empty);
            }

            /** Trailing **/
            if (underlayngType.HasTrailingTrivia)
            {
                //  Save it
                trailingTrivia = underlayngType.GetTrailingTrivia();

                //  Remove it from the underlaying type
                underlayngType = underlayngType.WithTrailingTrivia(SyntaxTriviaList.Empty);
            }

            //-------------------------------------------------------
            // Do we return value? if so, add our original sync return type as a type parameter
            //-------------------------------------------------------
            if ((underlayngType is PredefinedTypeSyntax)
                && ((PredefinedTypeSyntax)underlayngType).Keyword.IsKind(SyntaxKind.VoidKeyword))
            {
                //  The Sync method returns void - so we need to return Task.
                wrapperType = SyntaxFactory.ParseTypeName(TaskClassName).WithTrailingTrivia(SyntaxFactory.ElasticSpace);
            }
            else
            {
                // The Sync method return some type - so we need to return Task<T>.
                /* I'm using ParseTypeName instead of GenericName() since for some reason it won't allow me to simplify the type. */
                wrapperType = SyntaxFactory.ParseTypeName(string.Format(GenericTaskClassName, underlayngType.ToFullString()))
                    .WithAdditionalAnnotations(Simplifier.Annotation)
                    .WithTrailingTrivia(SyntaxFactory.ElasticSpace);

                //wrapperType = SyntaxFactory.GenericName(SyntaxFactory.Identifier(TaskClassName), SyntaxFactory.TypeArgumentList(
                //    arguments: SyntaxFactory.SeparatedList(new[] { underlayngType.WithAdditionalAnnotations(Simplifier.Annotation) })
                //    )).WithTrailingTrivia(SyntaxFactory.ElasticSpace).WithAdditionalAnnotations(Simplifier.Annotation);
            }

            //-------------------------------------------------------
            //  Restore the trivia by transferring it to the wrapperType
            //-------------------------------------------------------

            /** Leading **/
            if (!leadingTrivia.Equals(SyntaxTriviaList.Empty))
            {
                wrapperType = wrapperType.WithLeadingTrivia(leadingTrivia);
            }

            /** Trailing **/
            if (!trailingTrivia.Equals(SyntaxTriviaList.Empty))
            {
                wrapperType = wrapperType.WithTrailingTrivia(trailingTrivia);
            }

            //-------------------------------------------------------
            //  Done.
            //-------------------------------------------------------
            //return SyntaxFactory.ParseTypeName("global::System.Collections.Generic.List<int>").WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticSpace)).WithAdditionalAnnotations(Simplifier.Annotation);
            return wrapperType.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticSpace)).WithAdditionalAnnotations(Simplifier.Annotation);
        }

        /// <summary>
        /// Converts the task representation into its underlaying type
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="underlayngType">The task representation.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The underlaying type.</returns>
        private static TypeSyntax ConvertToSyncType(Document document, TypeSyntax wrapperType, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            TypeSyntax underlayingType = wrapperType;
            //  The underlaying type holds the method trailing trivia (which is typicly the method documentation)
            //  So lets transfer it.
            var leadingTrivia = SyntaxTriviaList.Empty;
            var trailingTrivia = SyntaxTriviaList.Empty;

            /** Leading **/
            if (wrapperType.HasLeadingTrivia)
            {
                //  Save it
                leadingTrivia = wrapperType.GetLeadingTrivia();
            }

            /** Trailing **/
            if (wrapperType.HasTrailingTrivia)
            {
                //  Save it
                trailingTrivia = wrapperType.GetTrailingTrivia();
            }


            //-------------------------------------------------------
            //  Get the task class
            //-------------------------------------------------------

            if (wrapperType is QualifiedNameSyntax)
            {
                underlayingType = ((QualifiedNameSyntax)wrapperType).Right;
            }

            //-------------------------------------------------------
            //  Are we dealing with generic type?
            //-------------------------------------------------------

            if (underlayingType is GenericNameSyntax)
            {
                //-------------------------------------------------------
                //  Return the type argument
                //-------------------------------------------------------

                System.Diagnostics.Debug.Assert(((GenericNameSyntax)underlayingType).TypeArgumentList.Arguments.Count > 0);
                underlayingType = ((GenericNameSyntax)underlayingType).TypeArgumentList.Arguments.ElementAt(0);
            }
            else
            {
                underlayingType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword));
            }

            //-------------------------------------------------------
            //  Restore the trivia by transferring it to the wrapperType
            //-------------------------------------------------------

            /** Leading **/
            if (!leadingTrivia.Equals(SyntaxTriviaList.Empty))
            {
                underlayingType = underlayingType.WithLeadingTrivia(leadingTrivia);
            }

            /** Trailing **/
            if (!trailingTrivia.Equals(SyntaxTriviaList.Empty))
            {
                underlayingType = underlayingType.WithTrailingTrivia(trailingTrivia);
            }

            //-------------------------------------------------------
            //  We're dealing with void-return type
            //-------------------------------------------------------

            return underlayingType
                    .WithAdditionalAnnotations(Simplifier.Annotation)
                    .WithTrailingTrivia(SyntaxFactory.ElasticSpace);
        }

        /// <summary>
        /// Adds a paramter to the documentation trivia
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="trivia">The trivia to search the documentation in.</param>
        /// <param name="parameterName">The parameter name.</param>
        /// <param name="parameterDescription">The parameter description.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The modified trivia.</returns>
        private static SyntaxTriviaList AddParameterToDocumentationTrivia(Document document, SyntaxTriviaList trivia, string parameterName, string parameterDescription, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Does that trivia contains documentation comment?
            //-------------------------------------------------------

            if (!trivia.Any(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia)))
            {
                return trivia;
            }

            //-------------------------------------------------------
            //  Build the comment
            //-------------------------------------------------------
            var newParameterTag = SyntaxFactory.XmlElement(
                    startTag: SyntaxFactory.XmlElementStartTag(
                        name: SyntaxFactory.XmlName("param"),
                        attributes: SyntaxFactory.List(new List<XmlAttributeSyntax>()
                        {
                            SyntaxFactory.XmlNameAttribute(
                                startQuoteToken: SyntaxFactory.Token(SyntaxKind.DoubleQuoteToken),
                                name: SyntaxFactory.XmlName("name").WithLeadingTrivia(SyntaxFactory.ElasticSpace),
                                identifier: SyntaxFactory.IdentifierName(parameterName),
                                endQuoteToken: SyntaxFactory.Token(SyntaxKind.DoubleQuoteToken))
                        })
                    ),
                    content: SyntaxFactory.List(new List<XmlNodeSyntax>()
                    {
                        SyntaxFactory.XmlText(SyntaxFactory.TokenList(
                            SyntaxFactory.XmlTextLiteral(SyntaxTriviaList.Empty, parameterDescription, parameterDescription, SyntaxTriviaList.Empty)
                            ))
                    }),
                    endTag: SyntaxFactory.XmlElementEndTag(SyntaxFactory.XmlName("param"))
                )
                .WithLeadingTrivia(
                    SyntaxFactory.TriviaList(
                        SyntaxFactory.ElasticCarriageReturn,
                        SyntaxFactory.ElasticTab,
                        SyntaxFactory.SyntaxTrivia(SyntaxKind.DocumentationCommentExteriorTrivia, "/// ")
                        )
                    );

            //-------------------------------------------------------
            //  Gets the comment trivia
            //-------------------------------------------------------
            var commentTrivia = trivia.First(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));
            var commentTriviaIndex = trivia.IndexOf(commentTrivia);

            //-------------------------------------------------------
            //  Gets the actual structure
            //-------------------------------------------------------
            var commentTriviaStructure = commentTrivia.GetStructure();

            //-------------------------------------------------------
            //  We need to inject the parameter, so firstly verify if this XML has param tag.
            //-------------------------------------------------------
            var lastParamTag = commentTriviaStructure.ChildNodes().OfType<XmlElementSyntax>().LastOrDefault(n => n.StartTag.Name.LocalName.ValueText == "param");

            if (lastParamTag != null)
            {
                //-------------------------------------------------------
                //  Insert it after the last parameter tag
                //-------------------------------------------------------

                commentTriviaStructure = commentTriviaStructure.InsertNodesAfter(lastParamTag, new List<XmlNodeSyntax>() { newParameterTag });
            }
            else
            {
                //-------------------------------------------------------
                //  We don't have any tag, so attempt to insert it after the summary tag
                //-------------------------------------------------------
                var summaryTag = commentTriviaStructure.ChildNodes().OfType<XmlElementSyntax>().FirstOrDefault(n => n.StartTag.Name.LocalName.ValueText == "summary");
                if (summaryTag != null)
                {
                    commentTriviaStructure = commentTriviaStructure.InsertNodesAfter(summaryTag, new List<XmlNodeSyntax>() { newParameterTag });
                }
                else
                {
                    //-------------------------------------------------------
                    //  If for some reason we can't find it, try to find the returns tag.
                    //-------------------------------------------------------
                    var returnsTag = commentTriviaStructure.ChildNodes().OfType<XmlElementSyntax>().FirstOrDefault(n => n.StartTag.Name.LocalName.ValueText == "returns");
                    if (returnsTag != null)
                    {
                        commentTriviaStructure = commentTriviaStructure.InsertNodesAfter(returnsTag, new List<XmlNodeSyntax>() { newParameterTag });
                    }
                }
            }

            var restoredTrivia = SyntaxFactory.SyntaxTrivia(SyntaxKind.DocumentationCommentExteriorTrivia, commentTriviaStructure.ToFullString());
            trivia = trivia.Replace(trivia[commentTriviaIndex], restoredTrivia);

            return SyntaxFactory.ParseLeadingTrivia(trivia.ToFullString());//.NormalizeWhitespace(elasticTrivia: true);
            //return SyntaxFactory.ParseLeadingTrivia(commentTriviaStructure.ToFullString()); // I don't like that we need to perform parsing. We should find alternative way.
        }

        /// <summary>
        /// Removes a paramter from the documentation trivia
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="trivia">The trivia to search the documentation in.</param>
        /// <param name="parameterName">The parameter name to look for.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The modified trivia.</returns>
        private static SyntaxTriviaList RemoveParameterFromDocumentationTrivia(Document document, SyntaxTriviaList trivia, string parameterName, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Does that trivia contains documentation comment?
            //-------------------------------------------------------

            if (!trivia.Any(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia)))
            {
                return trivia;
            }

            //-------------------------------------------------------
            //  Gets the comment trivia
            //-------------------------------------------------------
            var commentTrivia = trivia.First(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));
            var commentTriviaIndex = trivia.IndexOf(commentTrivia);

            //-------------------------------------------------------
            //  Gets the actual structure
            //-------------------------------------------------------
            var commentTriviaStructure = commentTrivia.GetStructure();

            //-------------------------------------------------------
            //  We need to inject the parameter, so firstly verify if this XML has param tag.
            //-------------------------------------------------------
            var paramTag = commentTriviaStructure.ChildNodes().OfType<XmlElementSyntax>().FirstOrDefault(
                n => n.StartTag.Name.LocalName.ValueText == "param" && n.StartTag.Attributes.Count > 0
                && n.StartTag.Attributes.OfType<XmlNameAttributeSyntax>().Any(
                    a => a.Name.LocalName.ValueText == "name" && ((XmlNameAttributeSyntax)a).Identifier.ToFullString() == parameterName
                    )
             );

            //-------------------------------------------------------
            //  Remove the tag if we've found it
            //-------------------------------------------------------
            if (paramTag != null)
            {
                /* Fetches the preffix node ("{tab}/// ") */
                var preffixNode = commentTriviaStructure.FindNode(new Microsoft.CodeAnalysis.Text.TextSpan(paramTag.Span.Start - 1, 1));

                /* Remove the prefix and the param tag */
                commentTriviaStructure = commentTriviaStructure.RemoveNodes(new[] { preffixNode, paramTag }, SyntaxRemoveOptions.KeepNoTrivia);
            }

            //-------------------------------------------------------
            //  Rebuild
            //-------------------------------------------------------
            var restoredTrivia = SyntaxFactory.SyntaxTrivia(SyntaxKind.DocumentationCommentExteriorTrivia, commentTriviaStructure.ToFullString());
            trivia = trivia.Replace(trivia[commentTriviaIndex], restoredTrivia);

            return SyntaxFactory.ParseLeadingTrivia(trivia.ToFullString());
        }

        #endregion

    }
}
