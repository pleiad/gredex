export default function Syntax({ showLabels }: { showLabels: boolean }) {
  return (
    <div>
      <table>
        <tbody>
          <tr>
            <td>T</td>
            <td>::=</td>
            <td>
              int | bool | T {"->"} T | A x B | {"★"}{" "}
            </td>
            <td>{showLabels ? "(Gradual Types)" : ""}</td>
          </tr>
          <tr>
            <td>b</td>
            <td>::=</td>
            <td>true | false</td>
            <td>{showLabels ? "(Booleans)" : ""}</td>
          </tr>
          <tr>
            <td>n</td>
            <td>::=</td>
            <td>natural numbers</td>
            <td>{showLabels ? "(Natural numbers)" : ""}</td>
          </tr>
          <tr>
            <td>binop</td>
            <td>::=</td>
            <td>
              + | - | {"<"} | {">"} | {"="} | {"*"} | {"∧"} | {"∨"}
            </td>
            <td>{showLabels ? "(Binary operations)" : ""}</td>
          </tr>
          <tr>
            <td>unop</td>
            <td>::=</td>
            <td>¬</td>
            <td>{showLabels ? "(Unary operations)" : ""}</td>
          </tr>
          <tr>
            <td>e</td>
            <td>::=</td>
            <td>
              x | b | n | () | {"e binop e"} | {"unop e"} |
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td></td>
            <td>if e then e else e |</td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td></td>
            <td>
              (λx.e) | (λx:T.e) | {"e e"} | {"fix x. e"} |
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td></td>
            <td>
              {" "}
              {"(e,e)"} | {"fst e"} | {"snd e"} |
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td></td>
            <td> {"{let x = e in e}"} |</td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td></td>
            <td>
              {" "}
              {"inl {T} e"} | {"inr {T} e"} | {"case e of {x => e} {x => e}"} |
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td></td>
            <td> e :: A </td>
            <td>{showLabels ? "(Expressions)" : ""}</td>
          </tr>
        </tbody>
      </table>
      {'To write the "λ" character, type "\\"'}.<br />
      {'To write the "★" character, type "?"'}.<br />
      {'To write the "¬" character, type "not"'}.<br />
      {'To write the "∧" character, type "and"'}.<br />
      {'To write the "∨" character, type "or"'}.<br />
    </div>
  );
}
