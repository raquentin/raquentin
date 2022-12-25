import { useParams } from "react-router-dom";

import Eatcode from "./eatcode/Eatcode"
import AppNotFound from "./AppNotFound";

export default function AppsRouter() {
    let { id } = useParams()

    switch (id) {
        case "eatcode":
            return <Eatcode />
        default:
            return <AppNotFound />
    }
}