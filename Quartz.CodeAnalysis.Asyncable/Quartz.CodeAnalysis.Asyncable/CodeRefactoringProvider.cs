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

using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace Quartz.CodeAnalysis.Asyncable
{
    [ExportCodeRefactoringProvider(CodeRefactoringProvider.RefactoringId, LanguageNames.CSharp)]
    internal class CodeRefactoringProvider : ICodeRefactoringProvider
    {
        public const string RefactoringId = "Quartz.CodeAnalysis.Asyncable";

        /// <summary>
        /// Entry point: gets the refactoring actions available for the current textspan.
        /// </summary>
        /// <param name="document">The document.</param>
        /// <param name="textSpan">The textspan.</param>
        /// <param name="cancellationToken">A cancellation token used to cancel this operation.</param>
        /// <returns>A list of <see cref="CodeAction"/> available for this textspan.</returns>
        public async Task<IEnumerable<CodeAction>> GetRefactoringsAsync(Document document, TextSpan textSpan, CancellationToken cancellationToken)
        {
            //-------------------------------------------------------
            //  Setup
            //-------------------------------------------------------

            /* Get the syntax tree root node */
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            /* Find the node at the selection. */
            var node = root.FindNode(textSpan);

            /* Create a semantic model since almost each method needs it */
            var semanticModel = await document.GetSemanticModelAsync();

            //-------------------------------------------------------
            //  What we're dealing with?
            //-------------------------------------------------------

            /******* Method Declaration *******/
            var methodDecl = node as MethodDeclarationSyntax;
            if (methodDecl != null)
            {
                /* This is an async or sync method? */
                if (await AsyncAnalysis.IsAsyncMethod(document, methodDecl, cancellationToken))
                {
                    /* Async - check if we got a sync version. If not, suggest to create one */
                    if (!await AsyncAnalysis.MethodHasSyncVariant(document, methodDecl, semanticModel, cancellationToken))
                    {
                        var action = CodeAction.Create("Create synchronous variant", c => CreateMethodSyncVariant(document, methodDecl, semanticModel, c));
                        return new[] { action };
                    }
                }
                else
                {
                    /* Sync - check if we got an async version. If not, suggest to create one */
                    if (!await AsyncAnalysis.MethodHasAsyncVariant(document, methodDecl, semanticModel, cancellationToken))
                    {
                        var action = CodeAction.Create("Create asynchronous variant", c => CreateMethodAsyncVariant(document, methodDecl, semanticModel, c));
                        return new[] { action };
                    }
                }
            }

            /******* Class Declaration *******/
            var classDecl = node as ClassDeclarationSyntax;
            if (classDecl != null)
            {
                if (await ClassDeclarationHasMissingVariant(document, classDecl, semanticModel, cancellationToken))
                {
                    var action = CodeAction.Create("Implement missing sync/async method variants.", c => CompleteClassMissingMethodVariants(document, classDecl, semanticModel, c));
                    return new[] { action };
                }
            }


            /******* Interface Declaration *******/
            var interfaceDecl = node as InterfaceDeclarationSyntax;
            if (interfaceDecl != null)
            {
                if (await InterfaceDeclarationHasMissingVariant(document, interfaceDecl, semanticModel, cancellationToken))
                {
                    var action = CodeAction.Create("Add missing sync/async method variant signatures.", c => CompleteInterfaceMissingMethodVariants(document, interfaceDecl, semanticModel, c));
                    return new[] { action };
                }
            }

            //-------------------------------------------------------
            //  Nothing imporant for us
            //-------------------------------------------------------
            return new List<CodeAction>();
        }

        #region Refactoring

        /// <summary>
        /// Creates an async method variant for the given sync method.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The sync method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The modified document.</returns>
        private async Task<Document> CreateMethodAsyncVariant(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //  Setup
            var originalSolution = document.Project.Solution;
            MethodDeclarationSyntax asyncMethodDecl;

            //  Are we dealing with interface or class?
            var typeDecl = methodDecl.Ancestors().OfType<TypeDeclarationSyntax>().FirstOrDefault();

            if (typeDecl is ClassDeclarationSyntax)
            {
                //  Create the async method
                asyncMethodDecl = await CodeGeneration.ImplementAsyncMethodVariant(document, methodDecl, semanticModel, cancellationToken);
            }
            else
            {
                //  Create the async method signature
                asyncMethodDecl = await CodeGeneration.ImplementAsyncMethodVariantSignature(document, methodDecl, semanticModel, cancellationToken);
            }


            //  Update the solution
            var root = await document.GetSyntaxRootAsync(cancellationToken);

            //  Insert
            document = document.WithSyntaxRoot(root.InsertNodesAfter(methodDecl, new[] { asyncMethodDecl }));

            //  Format
            document = await Formatter.FormatAsync(document, Formatter.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);
            //document = await Simplifier.ReduceAsync(document, Simplifier.Annotation, cancellationToken: cancellationToken);
            document = await Simplifier.ReduceAsync(document, asyncMethodDecl.FullSpan, null, cancellationToken).ConfigureAwait(false);

            return document;
        }

        /// <summary>
        /// Creates a sync method variant for the given async method.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="methodDecl">The async method.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The modified document.</returns>
        private async Task<Document> CreateMethodSyncVariant(Document document, MethodDeclarationSyntax methodDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            //  Setup
            MethodDeclarationSyntax syncMethodDecl;

            //  Are we dealing with interface or class?
            var typeDecl = methodDecl.Ancestors().OfType<TypeDeclarationSyntax>().FirstOrDefault();

            if (typeDecl is ClassDeclarationSyntax)
            {
                //  Create the async method signature
                syncMethodDecl = await CodeGeneration.ImplementSyncMethodVariant(document, methodDecl, semanticModel, cancellationToken);
            }
            else
            {
                //  Create the async method signature
                syncMethodDecl = await CodeGeneration.ImplementSyncMethodVariantSignature(document, methodDecl, semanticModel, cancellationToken);
            }


            //  Update the solution
            var root = await document.GetSyntaxRootAsync(cancellationToken);

            //  Insert
            document = document.WithSyntaxRoot(root.InsertNodesBefore(methodDecl, new[] { syncMethodDecl }));

            //  Format
            document = await Formatter.FormatAsync(document, Formatter.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);
            document = await Simplifier.ReduceAsync(document, Simplifier.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);

            return document;
        }

        /// <summary>
        /// Add to the given interface its missing method(s) variant.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="interfaceDecl">The interface.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The modified document.</returns>
        private async Task<Document> CompleteInterfaceMissingMethodVariants(Document document, InterfaceDeclarationSyntax interfaceDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            var interfaceMethods = interfaceDecl
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>().ToList();

            if (interfaceMethods.Count < 1)
            {
                return document;
            }

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            /**
             * Note:
             * I'd rather add each method at the right place. However, currently it seems impossible (a bug, prehaps).
             * So we need to collect all of the methods and insert them in the end of the class/interface.
             */
            var methodsList = new List<MethodDeclarationSyntax>();

            foreach (var methodDecl in interfaceMethods)
            {
                if (await AsyncAnalysis.IsAsyncMethod(document, methodDecl, cancellationToken))
                {
                    if (!(await AsyncAnalysis.MethodHasSyncVariant(document, methodDecl, semanticModel, cancellationToken)))
                    {
                        // Create a sync variant
                        var syncVariant = await CodeGeneration.ImplementSyncMethodVariantSignature(document, methodDecl, semanticModel, cancellationToken);
                        methodsList.Add(syncVariant);
                    }
                }
                else
                {
                    if (!(await AsyncAnalysis.MethodHasAsyncVariant(document, methodDecl, semanticModel, cancellationToken)))
                    {
                        // Create an async variant
                        var asyncVariant = await CodeGeneration.ImplementAsyncMethodVariantSignature(document, methodDecl, semanticModel, cancellationToken);
                        methodsList.Add(asyncVariant);
                    }
                }
            }

            //  In case there's no carridge return trivia at the end of the last method declaration,
            // Add a carridge return trivia to the first method in the methodsList
            var lastMethod = interfaceDecl.Members.OfType<MethodDeclarationSyntax>().Last();
            var lastMethodTrailingTrivia = lastMethod.GetTrailingTrivia();

            if (!lastMethodTrailingTrivia.Contains(SyntaxFactory.CarriageReturn)
                && !lastMethodTrailingTrivia.Contains(SyntaxFactory.CarriageReturnLineFeed)
                && !lastMethodTrailingTrivia.Contains(SyntaxFactory.ElasticCarriageReturn)
                && !lastMethodTrailingTrivia.Contains(SyntaxFactory.ElasticCarriageReturnLineFeed))
            {
                methodsList[0] = methodsList.ElementAt(0).WithLeadingTrivia(methodsList.ElementAt(0).GetLeadingTrivia().WithCarriageReturn());
            }

            //  Insert
            root = root.InsertNodesAfter(interfaceDecl.Members.OfType<MethodDeclarationSyntax>().Last(), methodsList);
            document = document.WithSyntaxRoot(root);

            //  Format
            document = await Simplifier.ReduceAsync(document, Simplifier.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);
            document = await Formatter.FormatAsync(document, Formatter.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);

            return document;
        }

        /// <summary>
        /// Add to the given class its missing method(s) variant.
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="classDecl">The class.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>The modified document.</returns>
        private async Task<Document> CompleteClassMissingMethodVariants(Document document, ClassDeclarationSyntax classDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            var classMethods = classDecl
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>().ToList();

            if (classMethods.Count < 1)
            {
                return document;
            }

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            /**
             * Note:
             * I'd rather add each method at the right place. However, currently it seems impossible (a bug, prehaps).
             * So we need to collect all of the methods and insert them in the end of the class/interface.
             */
            var methodsList = new List<MethodDeclarationSyntax>();

            foreach (var methodDecl in classMethods)
            {
                if (await AsyncAnalysis.IsAsyncMethod(document, methodDecl, cancellationToken))
                {
                    if (!(await AsyncAnalysis.MethodHasSyncVariant(document, methodDecl, semanticModel, cancellationToken)))
                    {
                        // Create a sync variant
                        var syncVariant = await CodeGeneration.ImplementSyncMethodVariant(document, methodDecl, semanticModel, cancellationToken);
                        methodsList.Add(syncVariant);
                    }
                }
                else
                {
                    if (!(await AsyncAnalysis.MethodHasAsyncVariant(document, methodDecl, semanticModel, cancellationToken)))
                    {
                        // Create an async variant
                        var asyncVariant = await CodeGeneration.ImplementAsyncMethodVariant(document, methodDecl, semanticModel, cancellationToken);
                        methodsList.Add(asyncVariant);
                    }
                }
            }

            if (methodsList.Count < 1)
            {
                return document;
            }

            //  In case there's no carridge return trivia at the end of the last method declaration,
            // Add a carridge return trivia to the first method in the methodsList
            var lastMethod = classDecl.Members.OfType<MethodDeclarationSyntax>().Last();
            var lastMethodTrailingTrivia = lastMethod.GetTrailingTrivia();

            if (!lastMethodTrailingTrivia.Contains(SyntaxFactory.CarriageReturn)
                && !lastMethodTrailingTrivia.Contains(SyntaxFactory.CarriageReturnLineFeed)
                && !lastMethodTrailingTrivia.Contains(SyntaxFactory.ElasticCarriageReturn)
                && !lastMethodTrailingTrivia.Contains(SyntaxFactory.ElasticCarriageReturnLineFeed))
            {
                methodsList[0] = methodsList.ElementAt(0).WithLeadingTrivia(methodsList.ElementAt(0).GetLeadingTrivia().WithCarriageReturn());
            }

            //  Insert
            root = root.InsertNodesAfter(classDecl.Members.OfType<MethodDeclarationSyntax>().Last(), methodsList);
            document = document.WithSyntaxRoot(root);

            //  Format
            document = await Simplifier.ReduceAsync(document, Simplifier.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);
            document = await Formatter.FormatAsync(document, Formatter.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);

            return document;
        }

        #endregion

        #region Helpers

        /// <summary>
        /// Determine if the given class decaration has a missing method variant (e.g. it missing an async method for sync method or vise-versa)
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="classDecl">The class declaration.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>Boolean value indicates whether the class has missing method variant or not.</returns>
        private async Task<bool> ClassDeclarationHasMissingVariant(Document document, ClassDeclarationSyntax classDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            var classMethods = classDecl
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>();

            foreach (var method in classMethods)
            {
                if (await AsyncAnalysis.IsAsyncMethod(document, method, cancellationToken))
                {
                    if (!(await AsyncAnalysis.MethodHasSyncVariant(document, method, semanticModel, cancellationToken)))
                    {
                        return true;
                    }
                }
                else
                {
                    if (!(await AsyncAnalysis.MethodHasAsyncVariant(document, method, semanticModel, cancellationToken)))
                    {
                        return true;
                    }
                }
            }

            return false;
        }


        /// <summary>
        /// Determine if the given interface decaration has a missing method variant (e.g. it missing an async method for sync method or vise-versa)
        /// </summary>
        /// <param name="document">The code analysis document.</param>
        /// <param name="interfaceDecl">The interface declaration.</param>
        /// <param name="semanticModel">A semantic model to look for the type in.</param>
        /// <param name="cancellationToken">A cancellation token.</param>
        /// <returns>Boolean value indicates whether the interface has missing method variant or not.</returns>
        private async Task<bool> InterfaceDeclarationHasMissingVariant(Document document, InterfaceDeclarationSyntax interfaceDecl, SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            var interfaceMethods = interfaceDecl
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>();

            foreach (var method in interfaceMethods)
            {
                if (await AsyncAnalysis.IsAsyncMethod(document, method, cancellationToken))
                {
                    if (!(await AsyncAnalysis.MethodHasSyncVariant(document, method, semanticModel, cancellationToken)))
                    {
                        return true;
                    }
                }
                else
                {
                    if (!(await AsyncAnalysis.MethodHasAsyncVariant(document, method, semanticModel, cancellationToken)))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        #endregion
    }
}