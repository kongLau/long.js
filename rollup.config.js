import { terser } from "rollup-plugin-terser";
import ts from "@wessberg/rollup-plugin-ts";

export default [
    {
        input: "src/long.ts",
        output: {
            file: "dist/long.js",
            format: "umd",
            name: "Long"
        },
        plugins: [
            ts({
                browserslist: false
            })
        ]
    },
    {
        input: "src/long.ts",
        output: {
            file: "dist/long.min.js",
            format: "umd",
            name: "Long"
        },
        plugins: [
            ts({
                browserslist: false,
                hook: {
                    outputPath: ((path1, kind) =>
                        kind === "declaration"
                            ? path1.replace(/\.(esm|min)\.d\.ts/, ".d.ts")
                            : path1
                    )
                }
            }),
            terser()
        ]
    }
]