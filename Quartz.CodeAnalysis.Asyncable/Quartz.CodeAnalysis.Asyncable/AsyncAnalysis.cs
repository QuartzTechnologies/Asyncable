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
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace Quartz.CodeAnalysis.Asyncable
{
    internal static class AsyncAnalysis
    {
        #region MethodHasAsyncVariant

        /// <summary>
        /// Determine if a class or interface contains an async variant of the given method decl.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="method">The sync method.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>Boolean value indicates whether the method container (class or interface) got an async method variant or not.</returns>
        public static async Task<bool> MethodHasAsyncVariant(Document document, MethodDeclarationSyntax methodDecl, CancellationToken cancellationToken)
        {
            return await MethodHasAsyncVariant(
                document: document,
                semanticModel: await document.GetSemanticModelAsync(cancellationToken),
                methodDecl: methodDecl,
                cancellationToken: cancellationToken
                );
        }

        /// <summary>
        /// Determine if a class or interface contains an async variant of the given method decl.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="method">The sync method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>Boolean value indicates whether the method container (class or interface) got an async method variant or not.</returns>
        public static async Task<bool> MethodHasAsyncVariant(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            //  Name
            var sourceMethodName = methodDecl.Identifier.ValueText;

            //  Return type info
            var sourceMethodReturnType = semanticModel.GetTypeInfo(methodDecl.ReturnType);

            //  Do we return void?
            var sourceMethodReturnsVoid = false;
            if (methodDecl.ReturnType is PredefinedTypeSyntax)
            {
                sourceMethodReturnsVoid = ((PredefinedTypeSyntax)methodDecl.ReturnType).Keyword.IsKind(SyntaxKind.VoidKeyword);
            }

            /* Get the parent type declaration */
            var typeDecl = methodDecl.Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

            //-------------------------------------------------------
            //  Iterate over each method
            //-------------------------------------------------------
            foreach (MethodDeclarationSyntax iterMethodDecl in typeDecl.Members.Where(m => m is MethodDeclarationSyntax && !m.Equals(methodDecl)))
            {
                /* First, make sure that this is an async method. */
                if (!await IsAsyncMethod(document, iterMethodDecl, cancellationToken))
                {
                    continue;
                }

                /* I really don't like this comprasion, but I don't see any appropriate way
                to make sure that we're dealing with the exact method.
                So what we're going to do? we're going to search for the method name in the iter method name
                in addition to the "Async" word.
                I won't restrict it to convension such as "{0}Async" since I still
                would like to allow for some basic modification. */
                if (!iterMethodDecl.Identifier.ValueText.Contains(sourceMethodName)
                    || !iterMethodDecl.Identifier.ValueText.Contains("Async"))
                {
                    continue;
                }

                /* Now, lets check what is the return type, and make sure that the Task underlaying (generic) return type
                    is the same as the sync method. For example:
                    Task<bool> FooAsync()
                    bool Foo()
                    We should make sure that the underlaying type for FooAsync is bool. */

                // Get the underlaying type 
                var iterMethodUnwrappedReturnType = UnwrapType(iterMethodDecl.ReturnType);
                if (sourceMethodReturnsVoid)
                {
                    // If we're returning void in the source code, we must be Task.
                    if (iterMethodUnwrappedReturnType is GenericNameSyntax)
                    {
                        continue; // We do know that we got one of the Task variants, since we passed IsAsync method. So if we're got generic method, this is not a void method.
                    }
                }
                else
                {
                    /* Make sure that we're dealing with generic type */
                    if (!(iterMethodUnwrappedReturnType is GenericNameSyntax))
                    {
                        continue;
                    }

                    /* Get the return type */
                    var destinationMethodReturnType = semanticModel.GetTypeInfo(
                        ((GenericNameSyntax)iterMethodDecl.ReturnType).TypeArgumentList
                        .Arguments
                        .ElementAt(0)
                    );

                    /* Compare the return types */
                    if (!sourceMethodReturnType.Equals(destinationMethodReturnType))
                    {
                        continue;
                    }
                }

                /* Validate the method params list */
                if (!methodDecl.ParameterList.IsEquivalentTo(iterMethodDecl.ParameterList))
                {
                    continue;
                }

                /* All validation passed. We can assume that this is an async variant. */
                return true;
            }

            /* Nothing found. */
            return false;
        }

        #endregion

        #region MethodHasSyncVariant

        /// <summary>
        /// Determine if a class or interface contains a sync variant of the given async method decl.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="method">The async method.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>Boolean value indicates whether the method container (class or interface) got a sync method variant or not.</returns>
        public static async Task<bool> MethodHasSyncVariant(Document document, MethodDeclarationSyntax methodDecl, CancellationToken cancellationToken)
        {
            return await MethodHasSyncVariant(
                document: document,
                semanticModel: await document.GetSemanticModelAsync(cancellationToken),
                methodDecl: methodDecl,
                cancellationToken: cancellationToken
                );
        }


        /// <summary>
        /// Determine if a class or interface contains a sync variant of the given async method decl.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="method">The async method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>Boolean value indicates whether the method container (class or interface) got a sync method variant or not.</returns>
        public static async Task<bool> MethodHasSyncVariant(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            //  Name
            //
            //  Like we've done in the HasAsyncVariant, we excpect the Async method to have an "Async" suffix
            //  So lets try to resolve the method name.
            var exceptedMethodName = Regex.Replace(methodDecl.Identifier.ValueText, "(Async|TaskAsync)$", "", RegexOptions.Compiled | RegexOptions.CultureInvariant | RegexOptions.IgnoreCase);
            

            //  Return type info
            //
            //  Note: we always return a Task, since methodDecl is async method.
            //  so we need to check if we're dealing with generic task or not.
            //  If this is not a generic task - the original return type is void.
            //  Otherwise, we can use GetTypeInfo and inspect the type.
            var sourceMethodReturnsVoid = false;
            TypeInfo? sourceMethodResolvedReturnType = null;
            var sourceMethodReturnType = UnwrapType(methodDecl.ReturnType);
            if (methodDecl.ReturnType is GenericNameSyntax)
            {
                sourceMethodResolvedReturnType = semanticModel.GetTypeInfo(
                     ((GenericNameSyntax)methodDecl.ReturnType).TypeArgumentList.Arguments.ElementAt(0)
                    );
            }
            else
            {
                sourceMethodReturnsVoid = true;
            }

            /* Get the parent type declaration */
            var typeDecl = methodDecl.Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();


            //-------------------------------------------------------
            //  Iterate over each method
            //-------------------------------------------------------
            foreach (MethodDeclarationSyntax iterMethodDecl in typeDecl.Members.Where(m => m is MethodDeclarationSyntax && !m.Equals(methodDecl)))
            {
                /* First, make sure that this is a sync method. */
                if (!await IsSyncMethod(document, iterMethodDecl, cancellationToken))
                {
                    continue;
                }

                /* Check for the method name */
                if (!iterMethodDecl.Identifier.ValueText.Contains(exceptedMethodName))
                {
                    continue;
                }

                /* Check the sync method return type */
                if (sourceMethodReturnsVoid)
                {
                    //  Void return type is been evaluated as PredefinedTypeSyntax.
                    //  If we're returning anything other than that - this is not a variant.
                    if (!(methodDecl.ReturnType is PredefinedTypeSyntax))
                    {
                        continue;
                    }

                    //  The return type is void?
                    if (!((PredefinedTypeSyntax)methodDecl.ReturnType).Keyword.IsKind(SyntaxKind.VoidKeyword))
                    {
                        continue;
                    }
                }
                else
                {
                    /* Get the return type info */
                    var destinationMethodReturnType = semanticModel.GetTypeInfo(iterMethodDecl.ReturnType);

                    /* Compare the return types */
                    if (!sourceMethodResolvedReturnType.Value.Equals(destinationMethodReturnType))
                    {
                        continue;
                    }
                }

                /* Validate the method params list */
                if (!methodDecl.ParameterList.IsEquivalentTo(iterMethodDecl.ParameterList))
                {
                    continue;
                }

                /* All validation passed. We can assume that this is an async variant. */
                return true;
            }

            /* Nothing found. */
            return false;
        }

        #endregion

        /// <summary>
        /// Determine if the specified method is an async method or not.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The method.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>True if this is an async method, false otherwise.</returns>
        public static async Task<bool> IsAsyncMethod(Document document, MethodDeclarationSyntax methodDecl, CancellationToken cancellationToken)
        {
            /* There's a simple way to do that - if we got an "async" keyword, we can be sure that this's an async method. */
            if (methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AsyncKeyword)))
            {
                return true;
            }

            /* If we didn't got an async keyword, we should check for a System.Threading.Tasks.Task return type. */
            return await TypeInheritTask(document, methodDecl.ReturnType, cancellationToken);
        }

        /// <summary>
        /// Determine if the specified method is a sync method or not.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The method.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>True if this is an async method, false otherwise.</returns>
        public static async Task<bool> IsSyncMethod(Document document, MethodDeclarationSyntax methodDecl, CancellationToken cancellationToken)
        {
            /* There's a simple way to do that - if we got an "async" keyword, we can be sure that this's an async method. */
            if (methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AsyncKeyword)))
            {
                return false;
            }

            /* If we didn't got an async keyword, we should check for a System.Threading.Tasks.Task return type. */
            return !(await TypeInheritTask(document, methodDecl.ReturnType, cancellationToken));
        }

        #region TypeInheritTask

        /// <summary>
        /// Demernie if the given type does inherit <see cref="System.Threading.Tasks.Task"/> and therefore support async.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="typeDecl">The type to check</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>True if the type inheirts <see cref="System.Threading.Tasks.Task"/> or false otherwise.</returns>
        public static async Task<bool> TypeInheritTask(Document document, TypeSyntax typeDecl, CancellationToken cancellationToken)
        {
            return await TypeInheritTask(
                semanticModel: await document.GetSemanticModelAsync(cancellationToken),
                typeDecl: typeDecl,
                cancellationToken: cancellationToken
                );
        }

        /// <summary>
        /// Demernie if the given type does inherit <see cref="System.Threading.Tasks.Task"/> and therefore support async.
        /// </summary>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="typeDecl">The type to check</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>True if the type inheirts <see cref="System.Threading.Tasks.Task"/> or false otherwise.</returns>
        public static Task<bool> TypeInheritTask(SemanticModel semanticModel, TypeSyntax typeDecl, CancellationToken cancellationToken)
        {
            /* Get the type info */
            var typeInfo = semanticModel.GetTypeInfo(typeDecl, cancellationToken);
            ITypeSymbol type = typeInfo.ConvertedType;

            /* Iterate through the type parents and look for System.Threading.Tasks.Task. */
            while (type != null && !(type.Name == "object" && type.ContainingNamespace.ToDisplayString() == "System"))
            {
                /* We're dealing with Task? */
                if (type.Name == "Task" && type.ContainingNamespace.ToDisplayString() == "System.Threading.Tasks")
                {
                    return Task.FromResult<bool>(true);
                }
                
                /* Move forward */
                type = type.BaseType;
            }

            /* No... We're not inheriting from task */
            return Task.FromResult<bool>(false);
        }

        #endregion

        #region Helpers

        private static SimpleNameSyntax UnwrapType(TypeSyntax type)
        {
            if (type is QualifiedNameSyntax)
                return ((QualifiedNameSyntax)type).Right;
            return type as SimpleNameSyntax;
        }

        #endregion
    }
}
